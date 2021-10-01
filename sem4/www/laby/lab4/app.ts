import express, {NextFunction, Request, Response} from 'express';
import cookieParser from 'cookie-parser';
import logger from 'morgan';
import csurf from 'csurf';
import sqlite from 'sqlite3';
import session from 'express-session';
import bodyParser from 'body-parser';
import path from 'path';
import {NOT_FOUND, OK, UNAUTHORIZED} from 'http-status-codes';

const SQLiteStore = require('connect-sqlite3')(session);

import {Meme, MemeService, Price} from "./src/memeService";
import {UserService} from "./src/userService";
import {initDatabase} from "./src/dbUtils";

const app = express();

const db: sqlite.Database = new sqlite.Database('.memes');
const csrfProtection = csurf({ cookie: true });

(async () => {
  initDatabase(db);

  const userService: UserService = new UserService(db);
  const memeService: MemeService = new MemeService(db);

  await userService.addUser(1, "admin", "admin");
  await userService.addUser(2, "user", "user");

  await memeService.addMeme(new Meme(1, "Gold", "https://i.redd.it/h7rplf9jt8y21.png", 1000));
  await memeService.addMeme(new Meme(2, "Platinum", "http://www.quickmeme.com/img/90/90d3d6f6d527a64001b79f4e13bc61912842d4a5876d17c1f011ee519d69b469.jpg", 1100));
  await memeService.addMeme(new Meme(3, "Elite", "https://i.imgflip.com/30zz5g.jpg", 1300));


  app.set('../views', path.join(__dirname, 'views'));
  app.set('view engine', 'pug');

  app.use(bodyParser.urlencoded({ extended: true }));
  app.use(cookieParser('przepraszamZaSlabeRozwiazania : ('));

  app.use((req: Request, res: Response, next: NextFunction) => {
    req.db = db;
    next();
  });

  app.use(session({
    store: new SQLiteStore(),
    secret: 'przepraszamZaSlabeRozwiazania : (',
    cookie: { maxAge: 10 * 60 * 1000 },
    resave: true,
    saveUninitialized: true
  }));



  app.get('/', csrfProtection, (req: Request, res: Response) => {
    res.render('login', {
      title: "Zaloguj sie",
      csrfToken: req.csrfToken()
    });
    return res.status(OK).end();
  });

  app.post('/', csrfProtection, async (req: Request, res: Response) => {
    const login: string = req.body.login;
    const password: string = req.body.password;

    if (login && password) {
      const areUserCredentialsValid: boolean = await userService.areUserCredentialsValid(login, password);

      if (areUserCredentialsValid && req.session) {
        req.session.login = login;

        res.redirect('/memes');
        return res.status(OK).end();
      }
    }

    res.redirect('/');
    return res.status(UNAUTHORIZED).end();
  });

  app.get('/logout', (req: Request, res: Response) => {
    delete(req.session.login);

    res.redirect('/');
  });

  app.get('/memes', async (req: Request, res: Response) => {
    if (req.session && req.session.login) {
      const mostExpensiveMemes: Meme[] = await memeService.getMost3ExpensiveMemes();

      res.render('index', {
        title: 'Meme market',
        message: 'Hello there!',
        memes: mostExpensiveMemes,
      });
      return res.status(OK).end();
    }

    res.redirect('/');
    return res.status(UNAUTHORIZED).end();
  });

  app.get('/meme/:memeId', csrfProtection, async (req: Request, res: Response) => {
    if (req.session && req.session.login) {
      const memeId: number = parseInt(req.params.memeId);

      if (await memeService.isMemeDefined(memeId)) {
        const meme: Meme = await memeService.getMeme(memeId);

        res.render('meme', { meme: meme, csrfToken: req.csrfToken() });
        return res.status(OK).end();
      } else {
        res.redirect('/error');
        return res.status(NOT_FOUND).end();
      }
    }

    res.redirect('/');
    return res.status(UNAUTHORIZED).end();
  });

  app.post('/meme/:memeId', csrfProtection, async (req: Request, res: Response) => {
    if (req.session && req.session.login) {
      const memeId: number = parseInt(req.params.memeId);
      const price: number = parseInt(req.body.price);
      const user: string = req.session.login;

      if (await memeService.isMemeDefined(memeId) && isNaN(price) == false) {
        const newPrice: Price = new Price(price, user);

        await memeService.updateMemePrice(memeId, newPrice);

        res.redirect(`/meme/${memeId}`);
        return res.status(OK).end();
      } else {
        res.redirect('/error');
        return res.status(NOT_FOUND).end();
      }
    }

    res.redirect('/');
    return res.status(UNAUTHORIZED).end();
  });

  app.use((req: Request, res: Response, next: NextFunction) => {
    res.render('error')
  });

  app.use((err: Error, req: Request, res: Response, next: NextFunction) => {

    res.status(500);
    res.render('error')
  });

})();


module.exports = app;
