import {QuizJson, QuizQuestionWithAnswerJson, quizzesArray} from "../../resources/quizzesConfig.js";
import {QuizGuard} from "../typeguards/typeguards.js";

export class Quizzes {

  private readonly quizzes: Quiz[];
  private chosenQuiz: Quiz;

  public constructor() {
    this.quizzes = this.parseQuizzesJsonsArray(quizzesArray);
    this.validateQuizzesArrayLength();
    this.chosenQuiz = this.quizzes[0];
  }

  private validateQuizzesArrayLength() {
    if (this.quizzes.length == 0) {
      throw new Error("no quizzes added");
    }
  }

  public getQuizzesNames(): string[] {
    return this.quizzes
      .map(quiz => quiz.getName());
  }

  public getChosenQuiz(): Quiz {
    return this.chosenQuiz;
  }

  public updateChosenQuiz(quizName: string) {
    this.chosenQuiz = this.findQuiz(quizName);
  }

  private findQuiz(quizName: string) {
    const quiz = this.quizzes
      .find(quiz => quiz.hasName(quizName));

    if (quiz == undefined) {
      throw new Error("invalid quiz name");
    }

    return quiz;
  }

  private parseQuizzesJsonsArray(quizzesJsonsArray: string[]): Quiz[] {
    return quizzesJsonsArray
      .map(json => Quiz.fromJson(json));
  }
}


export class Quiz {
  private readonly quizJson: QuizJson;

  private constructor(quizJson: QuizJson) {
    this.quizJson = quizJson;
  }

  public static fromJson(quizJson: string): Quiz {
    const parsedQuizJson = JSON.parse(quizJson);

    if (QuizGuard.check(parsedQuizJson)) {
      return new Quiz(parsedQuizJson);
    } else {
      throw new Error("invalid quiz json format");
    }
  }

  public toJson(): string {
    return JSON.stringify(this.quizJson);
  }

  public getName(): string {
    return this.quizJson.name;
  }

  public hasName(name: string): boolean {
    return this.getName() == name;
  }

  public getIntroduction(): string {
    return this.quizJson.introduction;
  }

  public getQuestionsListForUserAnswers(): QuizQuestionWithAnswersAndTime[] {
    return this.quizJson
        .questionsWithAnswers
        .map(questionJson => QuizQuestionWithAnswersAndTime.fromQuizQuestionWithAnswerJson(questionJson));
  }

}


export class QuizQuestionWithAnswersAndTime {

  private readonly quizQuestionWithAnswerJson: QuizQuestionWithAnswerJson;
  private userAnswer: number;
  private doesUserAnsweredFlag: boolean;
  private answerTime: number;

  private constructor(quizQuestionWithAnswerJson: QuizQuestionWithAnswerJson) {
    this.quizQuestionWithAnswerJson = quizQuestionWithAnswerJson;
    this.userAnswer = 0;
    this.doesUserAnsweredFlag = false;
    this.answerTime = 0;
  }

  public static fromQuizQuestionWithAnswerJson(quizQuestionWithAnswerJson: QuizQuestionWithAnswerJson): QuizQuestionWithAnswersAndTime {
    return new QuizQuestionWithAnswersAndTime(quizQuestionWithAnswerJson);
  }

  public updateUserAnswer(userAnswer: number) {
    this.userAnswer = userAnswer;
    this.doesUserAnsweredFlag = true;
  }

  public removeUserAnswer() {
    this.doesUserAnsweredFlag = false;
  }

  public updateUserAnswerTime(answerTime: number) {
    this.answerTime = answerTime;
  }

  public getUserAnswerTime(): number {
    return this.answerTime;
  }

  public doesUserAnswered(): boolean {
    return this.doesUserAnsweredFlag;
  }

  public isUserAnswerCorrect(): boolean {
    return this.doesUserAnswered()
      && this.userAnswer == this.quizQuestionWithAnswerJson.answer;
  }

  public getQuestionText(): string {
    return this.quizQuestionWithAnswerJson.question;
  }

  public getWrongAnswerPenalty(): number {
    return this.quizQuestionWithAnswerJson.wrongAnswerPenalty;
  }

  public getUserAnswer(): number {
    return this.userAnswer;
  }
}
