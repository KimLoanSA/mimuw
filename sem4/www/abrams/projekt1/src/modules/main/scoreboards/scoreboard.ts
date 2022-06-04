import {QuizQuestionWithAnswersAndTime} from "../quizzes/quizzes.js";
import {QuizDetailedScoreboardGuard} from "../typeguards/typeguards.js";

export class QuizDetailedScoreboard {

  private questionsStatistics: QuestionStatistics[];
  private quizScore: QuizScore;

  private constructor(questionsStatistics: QuestionStatistics[]) {
    this.questionsStatistics = questionsStatistics;
    this.quizScore = new QuizScore(this.calculateResultWithPenalties());
  }

  public static fromQuizQuestionWithAnswersAndTime(questionsListWithUserAnswers: QuizQuestionWithAnswersAndTime[]): QuizDetailedScoreboard {
    return new QuizDetailedScoreboard(this.mapQuizQuestionWithAnswersAndTime(questionsListWithUserAnswers));
  }

  public static fromJson(quizDetailedScoreboardJson: string): QuizDetailedScoreboard {
    const parsedQuizDetailedScoreboardJson = JSON.parse(quizDetailedScoreboardJson);

    if (QuizDetailedScoreboardGuard.check(parsedQuizDetailedScoreboardJson)) {
      const questionsStatistics: QuestionStatistics[] = this.copyOfQuestionStatisticsArray(parsedQuizDetailedScoreboardJson.questionsStatistics);
      return new QuizDetailedScoreboard(questionsStatistics);
    } else {
      throw new Error("invalid scoreboard json format");
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

  public getQuizScore(): QuizScore {
    return this.quizScore;
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

  private static mapQuizQuestionWithAnswersAndTime(questionsListWithUserAnswers: QuizQuestionWithAnswersAndTime[]): QuestionStatistics[] {
    return QuizQuestionWithAnswersAndTimeMapper
      .mapToQuestionStatisticsArray(questionsListWithUserAnswers);
  }

  private calculateResultWithPenalties(): number {
    return this.questionsStatistics
      .map(questionStatistics => questionStatistics.getTimeWithPenalty())
      .reduce((sum, score) => sum + score);
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


export class QuestionStatistics {

  private readonly isAnswerCorrectFlag: boolean;
  private readonly timePenalty: number;
  private readonly timeSpendInSeconds: number;

  public constructor(isAnswerCorrect: boolean, timePenalty: number, timeSpendInSeconds: number) {
    this.isAnswerCorrectFlag = isAnswerCorrect;
    this.timePenalty = timePenalty;
    this.timeSpendInSeconds = timeSpendInSeconds;
  }

  public static copyOf(questionStatistics: QuestionStatistics): QuestionStatistics {
    return new QuestionStatistics(
        questionStatistics.isAnswerCorrectFlag,
        questionStatistics.timePenalty,
        questionStatistics.timeSpendInSeconds);
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

}


class QuizQuestionWithAnswersAndTimeMapper {

  public static mapToQuestionStatisticsArray(quizQuestionWithAnswersAndTimeArray: QuizQuestionWithAnswersAndTime[]): QuestionStatistics[] {
    return quizQuestionWithAnswersAndTimeArray
        .map(quizQuestionWithAnswersAndTime => this.mapToQuestionStatistics(quizQuestionWithAnswersAndTime));
  }

  private static mapToQuestionStatistics(quizQuestionWithAnswersAndTime: QuizQuestionWithAnswersAndTime): QuestionStatistics {
    const isAnswerCorrect: boolean = quizQuestionWithAnswersAndTime.isUserAnswerCorrect();
    const wrongAnswerPenalty: number = quizQuestionWithAnswersAndTime.getWrongAnswerPenalty();
    const answerTimeInSeconds: number = quizQuestionWithAnswersAndTime.getUserAnswerTime();

    return new QuestionStatistics(isAnswerCorrect, wrongAnswerPenalty, answerTimeInSeconds);
  }

}
