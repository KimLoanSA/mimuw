import cookieParser from 'cookie-parser';
import path from 'path';

import express, { Request, Response, NextFunction } from 'express';
import 'express-async-errors';

import session from 'express-session'
// tslint:disable-next-line:no-var-requires
const SQLiteStore = require('connect-sqlite3')(session);

import routes from './routes/routes';
import {csrfCookieSetter, csrfProtectionMiddleware} from './middlewares/csrfMiddleware';
import {isUserLoggedMiddleware} from './middlewares/userMiddleware';

const app = express();

app.use(express.json());
app.use(express.urlencoded({extended: false}));
app.use(cookieParser('pozdrawiamPanaCiebiere42'));
app.use(session({
    store: new SQLiteStore({dir:'persistence', db: 'sessions'}),
    secret: 'pozdrawiamPanaCiebiere42',
    cookie: { maxAge: 10 * 60 * 1000},
}));

app.use('/api', routes);

app.use('/static/user', csrfProtectionMiddleware, csrfCookieSetter, express.static(path.join(__dirname, 'public/html/login')));

app.use('/static/quiz', csrfProtectionMiddleware, csrfCookieSetter, isUserLoggedMiddleware, express.static(path.join(__dirname, 'public/html/quiz')));
app.use('/static/user', csrfProtectionMiddleware, csrfCookieSetter, isUserLoggedMiddleware, express.static(path.join(__dirname, 'public/html/user')));

app.use('/static/stylesheets', csrfProtectionMiddleware, csrfCookieSetter, express.static(path.join(__dirname, 'public/stylesheets')));
app.use('/static/scripts', csrfProtectionMiddleware, csrfCookieSetter, express.static(path.join(__dirname, 'public/scripts')));

app.use((req: Request, res: Response, next: NextFunction) => {
    res.redirect('/static/user/login.html');
});

app.listen(3000);

export default app;
