import {QuizQuestionWithAnswersAndTime} from '../quizzes/quizzes.js';
import {QuizDetailedScoreboardGuard} from '../typeguards/typeguards.js';

export class QuizDetailedScoreboard {

  private readonly questionsStatistics: QuestionStatistics[];
  private readonly quizScore: QuizScore;

  private constructor(questionsStatistics: QuestionStatistics[]) {
    this.questionsStatistics = questionsStatistics;
    this.quizScore = new QuizScore(this.calculateResultWithPenalties());
  }

  public static fromJson(quizDetailedScoreboardJson: string): QuizDetailedScoreboard {
    const parsedQuizDetailedScoreboardJson = JSON.parse(quizDetailedScoreboardJson);

    if (QuizDetailedScoreboardGuard.check(parsedQuizDetailedScoreboardJson)) {
      const questionsStatistics: QuestionStatistics[] = this.copyOfQuestionStatisticsArray(parsedQuizDetailedScoreboardJson.questionsStatistics);
      return new QuizDetailedScoreboard(questionsStatistics);
    } else {
      throw new Error('invalid scoreboard json format');
    }
  }

  private static copyOfQuestionStatisticsArray(questionsStatistics: QuestionStatistics[]): QuestionStatistics[] {
    return questionsStatistics
      .map(questionStatistics => QuestionStatistics.copyOf(questionStatistics));
  }

  public toJson(): string {
    return JSON.stringify(this);
  }

  public getNumericQuizScore(): number {
    return this.quizScore.getScore();
  }

  public getQuestionsStatistics(): QuestionStatistics[] {
    return this.questionsStatistics;
  }

  public getNumberOfCorrectsAnswers(): number {
    return this.questionsStatistics
        .map(questionStatistics => questionStatistics.isAnswerCorrect())
        .map(isAnswerCorrect => Number(isAnswerCorrect))
        .reduce((sum, isCorrect) => sum + isCorrect);
  }

  public getNumberOfAnswers(): number {
    return this.questionsStatistics.length;
  }

  private calculateResultWithPenalties(): number {
    return this.questionsStatistics
      .map(questionStatistics => questionStatistics.getTimeWithPenalty())
      .reduce((sum, score) => sum + score);
  }

}


export class QuestionStatistics {

  private readonly isAnswerCorrectFlag: boolean;
  private readonly timePenalty: number;
  private readonly timeSpendInSeconds: number;
  private readonly correctAnswer: number;
  private readonly averageCorrectAnswerTime: number;

  public constructor(isAnswerCorrect: boolean, timePenalty: number, timeSpendInSeconds: number, correctAnswer: number, averageCorrectAnswerTime: number) {
    this.isAnswerCorrectFlag = isAnswerCorrect;
    this.timePenalty = timePenalty;
    this.timeSpendInSeconds = timeSpendInSeconds;
    this.correctAnswer = correctAnswer;
    this.averageCorrectAnswerTime = averageCorrectAnswerTime;
  }

  public static copyOf(questionStatistics: QuestionStatistics): QuestionStatistics {
    return new QuestionStatistics(
        questionStatistics.isAnswerCorrectFlag,
        questionStatistics.timePenalty,
        questionStatistics.timeSpendInSeconds,
        questionStatistics.correctAnswer,
        questionStatistics.averageCorrectAnswerTime);
  }

  public isAnswerCorrect(): boolean {
    return this.isAnswerCorrectFlag;
  }

  public getAnswerTime(): number {
    return this.timeSpendInSeconds;
  }

  public getTimePenalty(): number {
    return this.timePenalty;
  }

  public getTimeWithPenalty(): number {
    if (this.isAnswerCorrectFlag) {
      return this.timeSpendInSeconds;
    } else {
      return this.timeSpendInSeconds + this.timePenalty;
    }
  }

  public getCorrectAnswer(): number {
    return this.correctAnswer;
  }

  public getAverageAnswerTime(): number {
    return this.averageCorrectAnswerTime;
  }

}


export class QuizPercentageTimeDetailedScoreboard {

  private readonly quizName: string;
  private readonly questionPercentageTimeStatistics: QuestionPercentageTimeStatistics[];

  private constructor(quizName: string, questionPercentageTimeStatistics: QuestionPercentageTimeStatistics[]) {
    this.quizName = quizName;
    this.questionPercentageTimeStatistics = questionPercentageTimeStatistics;
  }

  public static fromQuizQuestionWithAnswersAndTime(quizName: string, questionsListWithUserAnswers: QuizQuestionWithAnswersAndTime[]) {
    const questionPercentageTimeStatistic: QuestionPercentageTimeStatistics[] =
        QuizPercentageTimeDetailedScoreboard.mapQuestionsListWithUserAnswers(questionsListWithUserAnswers);

    return new QuizPercentageTimeDetailedScoreboard(quizName, questionPercentageTimeStatistic);
  }

  private static mapQuestionsListWithUserAnswers(questionsListWithUserAnswers: QuizQuestionWithAnswersAndTime[]): QuestionPercentageTimeStatistics[] {
    const wholeTime: number = QuizPercentageTimeDetailedScoreboard.calculateWholeTime(questionsListWithUserAnswers);

    return questionsListWithUserAnswers
      .map(questionsListWithUserAnswer => QuestionPercentageTimeStatistics.fromQuizQuestionWithAnswersAndTime(questionsListWithUserAnswer, wholeTime));
  }

  private static calculateWholeTime(questionsListWithUserAnswers: QuizQuestionWithAnswersAndTime[]): number {
    return questionsListWithUserAnswers
      .map(questionsListWithUserAnswer => questionsListWithUserAnswer.getUserAnswerTime())
      .reduce((value, sum) => sum + value);
  }

  public toJson(): string {
    return JSON.stringify(this);
  }

}


export class QuestionPercentageTimeStatistics {

  private readonly answer: number;
  private readonly timeSpendPercentage: number;

  private constructor( answer: number, timeSpendPercentage: number) {
    this.answer = answer;
    this.timeSpendPercentage = timeSpendPercentage;
  }

  public static fromQuizQuestionWithAnswersAndTime(questionsListWithUserAnswers: QuizQuestionWithAnswersAndTime, wholeTime: number) {
    const userAnswer: number = questionsListWithUserAnswers.getUserAnswer();
    const timeSpendPercentage: number = QuestionPercentageTimeStatistics
      .calculateTimeSpendPercentage(questionsListWithUserAnswers.getUserAnswerTime(), wholeTime);

    return new QuestionPercentageTimeStatistics(userAnswer, timeSpendPercentage);
  }

  private static calculateTimeSpendPercentage(timeSpendInSeconds: number, wholeTime: number): number {
    return timeSpendInSeconds * 100 / wholeTime;
  }

}


export class QuizScore {

  private readonly score: number;

  public constructor(score: number) {
    this.score = score;
  }

  public getScore(): number {
    return this.score;
  }

  public static copyOf(quizScore: QuizScore): QuizScore {
    return new QuizScore(quizScore.score);
  }

  public compare(quizScore: QuizScore): number {
    if (this.score < quizScore.score) {
      return -1;
    } else if (this.score > quizScore.score) {
      return 1;
    } else {
      return 0;
    }
  }

}
