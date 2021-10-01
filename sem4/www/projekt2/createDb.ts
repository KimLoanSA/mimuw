import sqlite3 from 'sqlite3'
import {hashPassword} from './src/main/utils/hashUtils';

const db = new sqlite3.Database('persistence/main.db');

const exampleQuiz: string = `{
  "name": "alkoholowo matematyczny quiz",
  "introduction": "Liczyć i pić każdy może!",
  "questionsWithAnswers": [
    {
      "question": "A - rok, w którym firma Polmos opracowała sposób masowej produkcji wódki Żubrówki</br>B - rok, w którym zaczęto produkować piwo Żubr</br>Wtedy (A - B) / 2 to",
      "answer": 79,
      "wrongAnswerPenalty": 1822
    },
    {
    "question": "R - rok, w którym sponsorem Tatrzańskiego Parku Narodowego zostało piwo z postacią wzorowaną Janosikiem</br>A - rok widniejący na butelce Soplicy</br>Wtedy (R + A) * 3 to",
    "answer": 11712,
    "wrongAnswerPenalty": 1822
    },
    {
    "question": "M - liczba woltów w wódce Barmańska</br>S - liczba rąk człowieka</br>Wtedy M ^ S to",
    "answer": 324,
    "wrongAnswerPenalty": 1822
    },
    {
    "question": "X - liczba łyków na ile powinno się pić piwo w popularnej grze imprezowej flanki</br>D - liczba piw w czteropaku piwa </br>Wtedy (X * D) + 30 to",
    "answer": 42,
    "wrongAnswerPenalty": 1822
    }
  ]
}`;

const emptyAverageTimeStats: string = `{
  "questionsAverageTimeScoreboard": [
    {
      "answersTime":0,
      "numberOfAnswers":0
    },
    {
      "answersTime":0,
      "numberOfAnswers":0
    },
    {
      "answersTime":0,
      "numberOfAnswers":0
    },
    {
      "answersTime":0,
      "numberOfAnswers":0
    }
   ]
 }`;


db.serialize(async () => {

  db.run('CREATE TABLE users (id INTEGER PRIMARY KEY, username TEXT, password_hash TEXT, password_generation INTEGER)');
  db.run('INSERT INTO users VALUES (1, ?, ?, 0)', ['user1', hashPassword('user1')]);
  db.run('INSERT INTO users VALUES (2, ?, ?, 0)', ['user2', hashPassword('user2')]);

  db.run('CREATE TABLE quizzes (id INTEGER PRIMARY KEY, name TEXT, quiz TEXT)');
  db.run('INSERT INTO quizzes VALUES (1, ?, ?)', ['alkoholowo matematyczny quiz', exampleQuiz]);

  db.run('CREATE TABLE scores (id INTEGER PRIMARY KEY AUTOINCREMENT, quiz_id INTEGER REFERENCES quizzes(id), score INTEGER, user_id INTEGER REFERENCES users(id), stats TEXT)');

  db.run('CREATE TABLE average_time (id INTEGER PRIMARY KEY, quiz_id INTEGER REFERENCES quizzes(id), stats TEXT)');
  db.run('INSERT INTO average_time VALUES (?, ?, ?)', [1, 1, emptyAverageTimeStats]);
  db.run('INSERT INTO average_time VALUES (?, ?, ?)', [2, 2, emptyAverageTimeStats]);

});

