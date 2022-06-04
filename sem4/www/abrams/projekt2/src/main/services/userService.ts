import {Request} from 'express';
import {databaseService, UserDB} from './databaseService';
import {comparePasswordWithHash, hashPassword} from '../utils/hashUtils';

export class UserService {

  public async loginUser(req: Request, username: string, password: string): Promise<boolean> {
    const user: UserDB = await databaseService.getUserWithName(username);

    if (user !== undefined) {
      const isPasswordCorrect = comparePasswordWithHash(password, user.password_hash);

      if (isPasswordCorrect) {
        return this.loginCorrectUser(req, user);
      }
    }

    return false;
  }

  private loginCorrectUser(req: Request, user: UserDB): boolean {
    if (req.session !== undefined) {
      req.session.username = user.username;
      req.session.userId = user.id;
      req.session.passwordGeneration = user.password_generation;

      return true;
    }

    return false;
  }


  public async logoutUser(req: Request): Promise<void> {
    if (req.session !== undefined) {
      return new Promise( (resolve, _) => req.session!.destroy(resolve));
    }
  }


  public async changeUserPassword(req: Request, username: string, oldPassword: string, newPassword: string, newPasswordConfirmation: string): Promise<boolean> {
    const user: UserDB = await databaseService.getUserWithName(username);

    if (newPassword === newPasswordConfirmation && user !== undefined) {
      return await this.changeUserPasswordWithConfirmedPassword(req, user, newPassword);
    }

    return false;
  }

  private async changeUserPasswordWithConfirmedPassword(req: Request, user: UserDB, newPassword: string): Promise<boolean> {
    if (req.session !== undefined) {
      const username: string = user.username;
      const newPasswordGeneration: number = user.password_generation + 1;
      req.session.passwordGeneration = newPasswordGeneration;

      await this.updateUserPasswordAndPasswordGeneration(username, newPasswordGeneration, newPassword);


      return true;
    }

    return false;
  }

  private async updateUserPasswordAndPasswordGeneration(username: string, newPasswordGeneration: number, newPassword: string) {
    const hashedNewPassword: string = hashPassword(newPassword);

    await databaseService.updateUserPassword(username, hashedNewPassword, newPasswordGeneration);
  }


  public async isUserLoggedWithLatestPassword(username: string, passwordGeneration: number): Promise<boolean> {
    const user: UserDB = await databaseService.getUserWithName(username);

    if (user !== undefined) {
      const userPasswordLatestPasswordGeneration: number = user.password_generation;

      return this.isUserPasswordLatestOne(userPasswordLatestPasswordGeneration, passwordGeneration);
    }

    return false;
  }

  private isUserPasswordLatestOne(userPasswordLatestPasswordGeneration: number, passwordGeneration: number): boolean {
    return userPasswordLatestPasswordGeneration === passwordGeneration;
  }
}

export const userService: UserService = new UserService();
