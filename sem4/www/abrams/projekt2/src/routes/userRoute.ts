import 'express-session';
import {Request, Response, Router} from 'express';
import {OK, UNAUTHORIZED} from 'http-status-codes';

import {csrfProtectionMiddleware} from '../middlewares/csrfMiddleware';
import {isUserLoggedMiddleware} from '../middlewares/userMiddleware';

import {userService} from '../main/services/userService';

const router = Router();

router.post('/login', csrfProtectionMiddleware, async (req: Request, res: Response) => {
  const username = req.body.username;
  const password = req.body.password;

  if (username && password) {
    const loginUserResult = await userService.loginUser(req, username, password);

    if (loginUserResult) {
      res.redirect('/static/quiz/quiz.html');
      return res.status(OK).end();
    }
  }

  res.redirect('/static/user/login.html');
  return res.status(UNAUTHORIZED).end();
});


router.get('/logout', async (req: Request, res: Response) => {
  if (req.session !== undefined) {
    await userService.logoutUser(req);

    res.redirect('/static/user/login.html');
    return res.status(OK).end();
  }

  res.redirect('/static/user/login.html');
  return res.status(UNAUTHORIZED).end();
});


router.post('/change', csrfProtectionMiddleware, isUserLoggedMiddleware, async (req: Request, res: Response) => {
  const oldPassword: string = req.body.oldPassword;
  const newPassword: string = req.body.newPassword;
  const newPasswordConfirmation: string = req.body.newPasswordConfirmation;
  const username: string = req.session!.username;

  if (newPassword.trim().length > 4 && oldPassword && newPassword && newPasswordConfirmation) {
    const changeUserPasswordResult = await userService.changeUserPassword(req, username, oldPassword, newPassword, newPasswordConfirmation);

    if (changeUserPasswordResult) {
      res.redirect('logout');
      return res.status(OK).end();
    }
  }

  res.redirect('/static/user/passwordChange.html');
  return res.status(UNAUTHORIZED).end();
});


export default router;
