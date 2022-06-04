import sqlite from 'sqlite3'
import {asyncDbGet, asyncDbRun} from "./dbUtils";
import {comparePasswordWithHash, hashPassword} from "./hashing";

export class UserService {

  private readonly database: sqlite.Database;

  constructor(database: sqlite.Database) {
    this.database = database;
  }

  public async addUser(id : number, username : string, password : string): Promise<void> {
    const hashedPassword: string = hashPassword(password);

    return asyncDbRun(this.database,
        "INSERT INTO users (id, username, password) VALUES (?, ?, ?)",
        [id, username, hashedPassword]);
  }

  public async areUserCredentialsValid(username: string, password: string): Promise<boolean> {
    const user: any = await asyncDbGet(this.database,
        "SELECT * FROM users WHERE username = ?", [username]);

    if (user !== undefined) {
      return comparePasswordWithHash(password, user.password);
    }

    return false;
  }
}
