import {
  AverageTimeStatsDB, DatabaseService,
  QuizIdDB,
  QuizScoreShortDB,
  QuizShortDB,
  QuizWithJsonDB,
  ScoreDB,
  ScoreStatsDB
} from './databaseService';
import {
  QuizAverageTimeScoreboard,
  QuizDetailedScoreboard,
  QuizPercentageTimeDetailedScoreboard
} from '../quiz/scoreboard';
import {Quiz} from '../quiz/quizzes';

const databaseService: DatabaseService = new DatabaseService();

class QuizService {

  public async getAllUnsolvedQuizzes(userId: number): Promise<QuizShortDB[]> {
    const all: QuizShortDB[]  = await databaseService.getAllQuizzesIdsAndNames();
    const solved: QuizScoreShortDB[] = await databaseService.getSolvedQuizzesIdsByUser(userId);

    return this.filterSolvedFromUnsolved(all, solved, true);
  }


  public async getAllSolvedQuizzes(userId: number): Promise<QuizShortDB[]> {
    const all: QuizShortDB[]  = await databaseService.getAllQuizzesIdsAndNames();
    const solved: QuizScoreShortDB[] = await databaseService.getSolvedQuizzesIdsByUser(userId);

    return this.filterSolvedFromUnsolved(all, solved, false);
  }

  private filterSolvedFromUnsolved(all: QuizShortDB[], solved: QuizScoreShortDB[], predicateResult: boolean): QuizShortDB[] {
    return all
      .filter(unsolvedQuiz => this.findInSolvedPredicate(solved, unsolvedQuiz) === predicateResult);
  }

  private findInSolvedPredicate(solved: QuizScoreShortDB[], unsolvedQuiz: QuizShortDB): boolean {
    return solved
      .find( solvedQuiz => solvedQuiz.quiz_id === unsolvedQuiz.id) === undefined;
  }


  public async getQuizzesScores(): Promise<ScoreDB[]> {
    return await databaseService.getAllScores();
  }


  public async getQuizWithName(quizName: string): Promise<QuizWithJsonDB> {
    return await databaseService.getQuizWithName(quizName)
  }


  public async saveQuizResult(userId: number, quizResultJson: any, answerTime: number): Promise<boolean> {
    const quizPercentageTimeDetailedScoreboard: QuizPercentageTimeDetailedScoreboard = QuizPercentageTimeDetailedScoreboard.copyOf(quizResultJson);
    const quizName: string = quizPercentageTimeDetailedScoreboard.getQuizName();

    const quizId: QuizIdDB = await databaseService.getQuizIdWithName(quizName);

    if (await databaseService.getUserQuizScore(quizId.id, userId) === undefined) {
      const quizJson: QuizWithJsonDB = await databaseService.getQuizWithName(quizName);

      const quiz = Quiz.fromJson(quizJson.quiz);
      const quizDetailedScoreboard: QuizDetailedScoreboard =
          QuizDetailedScoreboard.fromQuizAndQuizPercentageTimeDetailedScoreboard(quiz, quizPercentageTimeDetailedScoreboard, answerTime);

      const averageTimeStatsDB: AverageTimeStatsDB = await databaseService.getAverageTimeStats(quizId.id);
      const quizAverageTimeScoreboard: QuizAverageTimeScoreboard = QuizAverageTimeScoreboard.fromJson(averageTimeStatsDB.stats);
      quizAverageTimeScoreboard.updateWithQuizScoreboard(quizDetailedScoreboard);

      await databaseService.saveAverageTimeStats(quizId.id, quizAverageTimeScoreboard.toJson());
      await databaseService.saveQuizScore(quizId.id, userId, quizDetailedScoreboard.getQuizScore().getScore(), quizDetailedScoreboard.toJson());

      return true;
    }

    return false;
  }


  public async getQuizResult(userId: number, quizName: string): Promise<QuizDetailedScoreboard> {
    const quizId: QuizIdDB = await databaseService.getQuizIdWithName(quizName);
    const userQuizScoreJson: ScoreStatsDB = await databaseService.getUserQuizScore(quizId.id, userId);
    const quizAverageTimeScoreboardJson: AverageTimeStatsDB = await databaseService.getAverageTimeStats(quizId.id);
    const quizAverageTimeScoreboard: QuizAverageTimeScoreboard = QuizAverageTimeScoreboard.fromJson(quizAverageTimeScoreboardJson.stats);

    const quizDetailedScoreboard: QuizDetailedScoreboard = QuizDetailedScoreboard.fromJson(userQuizScoreJson.stats);
    quizDetailedScoreboard.upadateAverageTime(quizAverageTimeScoreboard);

    return quizDetailedScoreboard;
  }


  public async getQuizTopScores(quizName: string): Promise<ScoreDB[]> {
    const quizId: QuizIdDB = await databaseService.getQuizIdWithName(quizName);

    return databaseService.getQuizTopScore(quizId.id);
  }


}


export const quizService: QuizService = new QuizService();
