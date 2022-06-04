import {NextFunction, Request, Response} from 'express';
import {userService} from '../main/services/userService';
import {UNAUTHORIZED} from 'http-status-codes';

export const isUserLoggedMiddleware = async (req: Request, res: Response, next: NextFunction) => {
  if (req.session !== undefined && req.session.username !== undefined) {
    const username: string = req.session.username;
    const passwordGeneration: number = req.session.passwordGeneration;

    const isUserLoggedWithLatestPasswordResult: boolean = await userService.isUserLoggedWithLatestPassword(username, passwordGeneration);

    if (isUserLoggedWithLatestPasswordResult) {
      next();
      return;
    }
  }

  res.redirect('/static/user/login.html');
  return res.status(UNAUTHORIZED).end();
};

