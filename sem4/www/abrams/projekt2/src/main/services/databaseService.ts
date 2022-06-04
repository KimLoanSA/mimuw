import {asyncDbAll, asyncDbGet, asyncDbRun} from '../utils/databaseUtils';
import sqlite3 from 'sqlite3';

export interface UserDB {
  id: number,
  username: string,
  password_hash: string,
  password_generation: number
}


export interface QuizShortDB {
  id: number,
  name: string
}

export interface QuizScoreShortDB {
  quiz_id: number
}

export interface QuizWithJsonDB {
  quiz: string
}

export interface QuizIdDB {
  id: number
}


export interface ScoreDB {
  score: number
}

export interface ScoreStatsDB {
  stats: string
}


export interface AverageTimeStatsDB {
  stats: string
}


export class DatabaseService {

  private database = new sqlite3.Database('persistence/main.db');


  public async getUserWithName(username: string): Promise<UserDB> {
    return asyncDbGet(this.database, 'SELECT * FROM users WHERE username = ?', [username]);
  }

  public async updateUserPassword(username: string, hashedNewPassword: string, newPasswordGeneration: number): Promise<void> {
    return asyncDbRun(this.database, 'UPDATE users SET password_hash = ?, password_generation = ? WHERE username = ?',
        [hashedNewPassword, newPasswordGeneration, username]);
  }


  public async getAllQuizzesIdsAndNames(): Promise<QuizShortDB[]> {
    return asyncDbAll(this.database, 'SELECT id, name FROM quizzes');
  }

  public async getQuizWithName(quizName: string): Promise<QuizWithJsonDB> {
    return asyncDbGet(this.database, 'SELECT quiz FROM quizzes WHERE name = ?', [quizName]);
  }

  public async getQuizIdWithName(quizName: string): Promise<QuizIdDB> {
    return asyncDbGet(this.database, 'SELECT id FROM quizzes WHERE name = ?', [quizName]);
  }


  public async getSolvedQuizzesIdsByUser(userId: number): Promise<QuizScoreShortDB[]> {
    return asyncDbAll(this.database,  'SELECT quiz_id FROM scores WHERE user_id = ?', [userId]);
  }

  public async getAllScores(): Promise<ScoreDB[]> {
    return asyncDbAll(this.database, 'SELECT score FROM scores');
  }

  public async saveQuizScore(quizId: number, userId: number, quizScore: number, quizStats: string): Promise<void> {
    return asyncDbRun(this.database, 'INSERT INTO scores (quiz_id, score, user_id, stats) VALUES (?, ?, ?, ?)', [quizId, quizScore, userId, quizStats])
  }

  public async getUserQuizScore(quizId: number, userId: number): Promise<ScoreStatsDB> {
    return asyncDbGet(this.database, 'SELECT stats FROM scores WHERE quiz_id = ? AND user_id = ?', [quizId, userId]);
  }

  public async getQuizTopScore(quizId: number): Promise<ScoreDB[]> {
    return asyncDbAll(this.database, 'SELECT score FROM scores WHERE quiz_id = ?', [quizId]);
  }


  public async saveAverageTimeStats(quizId: number, stats: string): Promise<void> {
    return asyncDbRun(this.database, 'INSERT OR REPLACE INTO average_time (id, quiz_id, stats) VALUES (?, ?, ?)', [quizId, quizId, stats]);
  }

  public async getAverageTimeStats(quizId: number): Promise<AverageTimeStatsDB> {
    return asyncDbGet(this.database, 'SELECT stats FROM average_time WHERE quiz_id = ?', [quizId]);
  }

}

export const databaseService: DatabaseService = new DatabaseService();
