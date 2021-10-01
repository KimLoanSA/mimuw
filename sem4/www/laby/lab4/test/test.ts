import { expect } from "chai";
import "mocha";
import {Meme, MemeService, Price} from "../src/memeService";
import sqlite from 'sqlite3';
import {initDatabase} from "../src/dbUtils";

describe("Meme tests", () => {

  it('latest price', () => {
    const latestPrice: number = 1;

    const meme = new Meme(1, "name", "url", latestPrice);

    expect(meme.getLatestPrice()).to.equal(latestPrice);
  });

  it('latest price update', () => {
    const price: number = 1;
    const latestPrice: number = 2;

    const meme = new Meme(1, "name", "url", price);
    meme.updatePrice(new Price(latestPrice, "user"));

    expect(meme.getLatestPrice()).to.equal(latestPrice);
  });

  it('price history', () => {
    const price1: number = 1;
    const price2: number = 2;
    const price3: number = 3;

    const meme = new Meme(1, "name", "url", price1);
    meme.updatePrice(new Price(price2, "user"));
    meme.updatePrice(new Price(price3, "user"));

    const priceHistory: Price[] = meme.getPriceHistory();

    expect(priceHistory.length).to.equal(3);
    expect(priceHistory[0].getPrice()).to.equal(price3);
    expect(priceHistory[1].getPrice()).to.equal(price2);
    expect(priceHistory[2].getPrice()).to.equal(price1);
  });

});

describe("MemeService test", () => {

  it('get meme with id', async () => {
    const memeService: MemeService = await initDbTest();

    const meme1 = new Meme(1, "test1", "test1", 1);
    const meme2 = new Meme(2, "test2", "test2", 2);
    const meme3 = new Meme(3, "test3", "test3", 3);

    await memeService.addMeme(meme1);
    await memeService.addMeme(meme2);
    await memeService.addMeme(meme3);

    const gotMeme: Meme = await memeService.getMeme(meme1.getId());
    expect(gotMeme.getId()).to.equal(meme1.getId());
  });

  it('is meme with id in db', async () => {
    const memeService: MemeService = await initDbTest();

    const meme1 = new Meme(1, "test1", "test1", 1);
    await memeService.addMeme(meme1);

    const isMeme1In: boolean = await memeService.isMemeDefined(meme1.getId());
    const isMeme2In: boolean = await memeService.isMemeDefined(2);

    expect(isMeme1In).to.equal(true);
    expect(isMeme2In).to.equal(false);
  });


  it('get top 3 most expensive memes', async () => {
    const memeService: MemeService = await initDbTest();

    const meme1 = new Meme(1, "test1", "test1", 1);
    const meme2 = new Meme(2, "test2", "test2", 2);
    const meme3 = new Meme(3, "test3", "test3", 3);

    await memeService.addMeme(meme1);
    await memeService.addMeme(meme2);
    await memeService.addMeme(meme3);

    let mostExpensiveMemes = await memeService.getMost3ExpensiveMemes();

    expect(mostExpensiveMemes.length).to.equal(3);
    expect(mostExpensiveMemes[0].getLatestPrice()).to.equal(meme3.getLatestPrice());
    expect(mostExpensiveMemes[1].getLatestPrice()).to.equal(meme2.getLatestPrice());
    expect(mostExpensiveMemes[2].getLatestPrice()).to.equal(meme1.getLatestPrice());
  });


  it('get top 3 most expensive memes, but 2 inserted', async () => {
    const memeService: MemeService = await initDbTest();

    const meme1 = new Meme(1, "test1", "test1", 1);
    const meme2 = new Meme(2, "test2", "test2", 2);

    await memeService.addMeme(meme1);
    await memeService.addMeme(meme2);

    let mostExpensiveMemes = await memeService.getMost3ExpensiveMemes();

    expect(mostExpensiveMemes.length).to.equal(2);
    expect(mostExpensiveMemes[0].getLatestPrice()).to.equal(meme2.getLatestPrice());
    expect(mostExpensiveMemes[1].getLatestPrice()).to.equal(meme1.getLatestPrice());
  });

});

async function initDbTest(): Promise<MemeService> {
  const db: sqlite.Database = new sqlite.Database(':memory:');
  initDatabase(db);
  return new MemeService(db);
}
