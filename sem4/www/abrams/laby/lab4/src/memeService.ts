import sqlite from 'sqlite3'
import {asyncDbAll, asyncDbGet, asyncDbRun} from "./dbUtils";

export class Meme {

  private readonly id: number;
  private readonly name: string;
  private readonly url: string;
  private readonly priceHistory: Price[];

  public constructor(id: number, name: string, url: string, price: number) {
    this.id = id;
    this.name = name;
    this.url = url;
    this.priceHistory = [];
    this.updatePrice(new Price(price, "-"));
  }

  public updatePrice(price: Price) {
    this.priceHistory
      .unshift(price);
  }

  public getLatestPrice(): number {
    return this.priceHistory[0]
      .getPrice();
  }

  public getId(): number {
    return this.id;
  }

  public getName(): string {
    return this.name;
  }

  public getUrl(): string {
    return this.url;
  }

  public getPriceHistory(): Price[] {
    return this.priceHistory;
  }

  public getPriceHistoryAsString(): string {
    return this.priceHistory
      .map(price => `${price.getPrice()};${price.getUser()}`)
      .join(",");
  }

  public compareByPrice(meme: Meme): number {
    return this.getLatestPrice() - meme.getLatestPrice();
  }
}


export class Price {

  private readonly price: number;
  private readonly user: string;

  constructor(price: number, user: string) {
    this.price = price;
    this.user = user;
  }

  public getPrice(): number {
    return this.price;
  }

  public getUser(): string {
    return this.user;
  }
}


export class MemeService {

  private readonly database: sqlite.Database;

  constructor(database: sqlite.Database) {
    this.database = database;
  }

  public async updateMemePrice(memeId: number, newPrice: Price): Promise<Meme> {
    await asyncDbRun(this.database, "BEGIN IMMEDIATE;");

    const meme: Meme | void = await this.getMeme(memeId)
      .catch(async () => {
        await asyncDbRun(this.database, "ROLLBACK;");
        return this.updateMemePrice(memeId, newPrice);
      });

    meme.updatePrice(newPrice);

    await this.addMeme(meme)
    .catch(async () => {
      await asyncDbRun(this.database, "ROLLBACK;");
      return this.updateMemePrice(memeId, newPrice);
    });

    await asyncDbRun(this.database, "COMMIT;")
    .catch(async () => {
      await asyncDbRun(this.database, "ROLLBACK;");
      return this.updateMemePrice(memeId, newPrice);
    });

    return meme;

  }

  public async addMeme(meme: Meme): Promise<void> {
    const id: number = meme.getId();
    const name: string = meme.getName();
    const url: string = meme.getUrl();
    const pricesString: string = meme.getPriceHistoryAsString();

    return asyncDbRun(this.database,
        "INSERT OR REPLACE INTO memes (id, name, url, prices) VALUES (?, ?, ?, ?)",
        [id, name, url, pricesString]);
  }

  public async getMost3ExpensiveMemes(): Promise<Meme[]> {
    const memesDb: any[] = await asyncDbAll(this.database, "SELECT * FROM memes");

    return memesDb
      .map(memeDb => this.mapMeme(memeDb))
      .sort((meme1: Meme, meme2: Meme) => meme2.compareByPrice(meme1))
      .slice(0, 3);
  }

  public async isMemeDefined(memeId: number): Promise<boolean> {
    const memeDb: any = await asyncDbGet(this.database,
        "SELECT * FROM memes WHERE id = ?", [memeId]);

    return memeDb !== undefined;
  }

  public async getMeme(memeId: number): Promise<Meme>  {
    const memeDb: any = await asyncDbGet(this.database,
        "SELECT * FROM memes WHERE id = ?", [memeId]);

    return this.mapMeme(memeDb);
  }

  private mapMeme(memeDb: any): Meme {
    const id: number = memeDb.id;
    const name: string = memeDb.name;
    const url: string = memeDb.url;
    const pricesString: string = memeDb.prices;
    const prices: Price[] = this.mapPrices(pricesString);

    const oldestPrice: number = prices.pop().getPrice();
    const meme: Meme = new Meme(id, name, url, oldestPrice);

    prices.reverse()
      .forEach(price => meme.updatePrice(price));

    return meme;
  }

  private mapPrices(pricesString: string): Price[] {
    return pricesString
      .split(",")
      .map(priceString => this.mapPrice(priceString));
  }

  private mapPrice(priceString: string): Price {
    const splittedPriceString: string[] = priceString.split(";");
    const price: number = parseInt(splittedPriceString[0], 10);
    const user: string = splittedPriceString[1];

    return new Price(price, user);
  }

}
