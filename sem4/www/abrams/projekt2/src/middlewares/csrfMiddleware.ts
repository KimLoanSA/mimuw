import csrf from 'csurf'
import {NextFunction, Request, Response} from 'express';

export const csrfProtectionMiddleware = csrf({ cookie: true });


export const csrfCookieSetter = (req: Request, res: Response, next: NextFunction) => {
  res.cookie('CSRF-TOKEN', req.csrfToken(), {secure: true});
  next();
};


