import {Quiz, QuizQuestionWithAnswersAndTime} from './quizzes.js';
import {QuizPercentageTimeDetailedScoreboard} from '../scoreboards/scoreboard.js';


export class QuizSession {

  private quiz: Quiz;
  private readonly questionsListWithUserAnswers: QuizQuestionWithAnswersAndTime[];
  private quizIndex: number;
  private sessionAnswersTime: number;

  private constructor(quiz: Quiz) {
    this.quiz = quiz;
    this.questionsListWithUserAnswers = quiz.getQuestionsListForUserAnswers();
    this.quizIndex = 0;
    this.sessionAnswersTime = 0;
  }

  public static startWithQuiz(quiz: Quiz): QuizSession {
    return new QuizSession(quiz);
  }

  public getQuizIntroduction(): string {
    return this.quiz.getIntroduction();
  }

  public updateSessionAnswersTime(answersTime: number) {
    this.sessionAnswersTime = answersTime;
  }

  public getSessionAnswersTime() {
    return this.sessionAnswersTime;
  }

  public updateUserAnswerTimeForCurrentQuestion(answerTime: number) {
    this.getCurrentQuestion()
      .updateUserAnswerTime(answerTime);
  }

  public getUserAnswerTimeForCurrentQuestion(): number {
    return this.getCurrentQuestion()
      .getUserAnswerTime();
  }

  public loadNextQuestion() {
    if (this.hasNextQuestion()) {
      this.quizIndex++;
    }
  }

  public hasNextQuestion() {
    return this.quizIndex + 1 < this.questionsListWithUserAnswers.length;
  }

  public loadPreviousQuestion() {
    if (this.hasPreviousQuestion()) {
      this.quizIndex--;
    }
  }

  public hasPreviousQuestion() {
    return this.quizIndex - 1 >= 0;
  }

  public getCurrentQuestionIndex(): number {
    return this.quizIndex + 1;
  }

  public getNumberOfAllQuestions(): number {
    return this.questionsListWithUserAnswers.length;
  }

  public getCurrentQuestionPenalty(): number {
    return this.getCurrentQuestion()
      .getWrongAnswerPenalty();
  }

  public getCurrentQuestionText(): string {
    return this.getCurrentQuestion()
      .getQuestionText();
  }

  private getCurrentQuestion(): QuizQuestionWithAnswersAndTime {
    return this.questionsListWithUserAnswers[this.quizIndex];
  }

  public updateUserAnswerForCurrentQuestion(userAnswer: number) {
    this.getCurrentQuestion()
      .updateUserAnswer(userAnswer);
  }

  public doesUserAnsweredForCurrentQuestion(): boolean {
    return this.getCurrentQuestion()
      .doesUserAnswered();
  }

  public getUserAnswerForCurrentQuestion(): number {
    return this.getCurrentQuestion()
      .getUserAnswer();
  }

  public removeUserAnswerForCurrentQuestion() {
    this.getCurrentQuestion()
      .removeUserAnswer();
  }

  public areAllQuestionsAnswered(): boolean {
    return this.questionsListWithUserAnswers
      .every(question => question.doesUserAnswered());
  }

  public getQuizPercentageTimeDetailedScoreboard(): QuizPercentageTimeDetailedScoreboard {
    return QuizPercentageTimeDetailedScoreboard
      .fromQuizQuestionWithAnswersAndTime(this.quiz.getName(), this.questionsListWithUserAnswers);
  }

}
