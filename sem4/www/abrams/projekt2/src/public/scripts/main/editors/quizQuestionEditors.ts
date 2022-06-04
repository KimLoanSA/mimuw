import {HTMLElementEditor} from './documentEditors.js';
import {QuizSession} from '../quizzes/quizSession.js';
import {QuizQuestionProperties} from '../properties/quizQuestionProperties.js';
import {Utils} from '../utils/utils.js';

export class CurrentQuizSessionPageEditor {

  private quizSession: QuizSession;
  private currentPageLoadTime: number;
  private currentQuestionAnswerTimeOnLoad: number;

  private paragraphEditor: HTMLElementEditor;
  private labelEditor: HTMLElementEditor;
  private questionInfoTableQuestionNumberEditor: HTMLElementEditor;
  private questionInfoTableAllQuestionsNumberEditor: HTMLElementEditor;
  private questionInfoTableQuizPageTimeEditor: HTMLElementEditor;
  private questionInfoTableQuizTimeEditor: HTMLElementEditor;
  private questionInfoTableTimePenaltyEditor: HTMLElementEditor;

  public constructor(document: Document, quizSession: QuizSession) {
    this.quizSession = quizSession;
    this.currentPageLoadTime = 0;
    this.currentQuestionAnswerTimeOnLoad = 0;

    this.paragraphEditor = new HTMLElementEditor(document, QuizQuestionProperties.QUIZ_QUESTION_INTRODUCTION_PARAGRAPH_ID);
    this.labelEditor = new HTMLElementEditor(document, QuizQuestionProperties.QUIZ_QUESTION_ANSWER_LABEL_ID);
    this.questionInfoTableQuestionNumberEditor = new HTMLElementEditor(document, QuizQuestionProperties.QUIZ_QUESTION_INFO_TABLE_QUESTION_NUMBER_ID);
    this.questionInfoTableAllQuestionsNumberEditor = new HTMLElementEditor(document, QuizQuestionProperties.QUIZ_QUESTION_INFO_TABLE_ALL_QUESTIONS_NUMBER_ID);
    this.questionInfoTableQuizPageTimeEditor = new HTMLElementEditor(document, QuizQuestionProperties.QUIZ_QUESTION_INFO_TABLE_QUIZ_PAGE_TIME_ID);
    this.questionInfoTableQuizTimeEditor = new HTMLElementEditor(document, QuizQuestionProperties.QUIZ_QUESTION_INFO_TABLE_QUIZ_TIME_ID);
    this.questionInfoTableTimePenaltyEditor = new HTMLElementEditor(document, QuizQuestionProperties.QUIZ_QUESTION_INFO_TABLE_TIME_PENALTY_ID);
  }

  public loadCurrentQuizSessionPage() {
    this.updateCurrentPageTimesOnLoad();
    this.updateCurrentQuestionStopwatch(this.currentPageLoadTime);

    this.loadCurrentQuizSessionPageQuizIntroduction();
    this.loadCurrentQuizSessionPageLabelText();

    this.loadCurrentQuizSessionPageQuestionNumber();
    this.loadCurrentQuizSessionPageNumberOfAllQuestions();

    this.loadCurrentQuizSessionPageWrongAnswerPenalty();
  }

  public updateQuizSessionTime(newStopwatchValue: number) {
    const formattedNewStopwatchValue = Utils.getStringDescriptingTimeInSeconds(newStopwatchValue);

    this.quizSession.updateSessionAnswersTime(newStopwatchValue);

    this.questionInfoTableQuizTimeEditor.setInnerHTML(formattedNewStopwatchValue);
  }

  public updateCurrentQuestionStopwatch(newStopwatchValue: number) {
    const realQuestionAnswerTime: number = this.currentQuestionAnswerTimeOnLoad - this.currentPageLoadTime + newStopwatchValue;
    const formattedNewStopwatchValue: string = Utils.getStringDescriptingTimeInSeconds(realQuestionAnswerTime);

    this.quizSession.updateUserAnswerTimeForCurrentQuestion(realQuestionAnswerTime);

    this.questionInfoTableQuizPageTimeEditor.setInnerHTML(formattedNewStopwatchValue);
  }

  private updateCurrentPageTimesOnLoad() {
    this.currentPageLoadTime = this.quizSession.getSessionAnswersTime();
    this.currentQuestionAnswerTimeOnLoad = this.quizSession.getUserAnswerTimeForCurrentQuestion();
  }

  private loadCurrentQuizSessionPageQuizIntroduction() {
    const quizIntroduction: string = this.quizSession.getQuizIntroduction();

    this.paragraphEditor.setInnerHTML(quizIntroduction);
  }

  private loadCurrentQuizSessionPageLabelText() {
    const actualQuizQuestionText: string = this.quizSession.getCurrentQuestionText();
    const formattedActualQuizQuestionText: string = `${actualQuizQuestionText}:</br>`;

    this.labelEditor.setInnerHTML(formattedActualQuizQuestionText);
  }

  private loadCurrentQuizSessionPageQuestionNumber() {
    const actualQuizQuestionNumber: number = this.quizSession.getCurrentQuestionIndex();
    const formattedActualQuizQuestionNumber: string = `${actualQuizQuestionNumber}`;

    this.questionInfoTableQuestionNumberEditor.setInnerHTML(formattedActualQuizQuestionNumber);
  }

  private loadCurrentQuizSessionPageNumberOfAllQuestions() {
    const actualQuizNumberOfAllQuestions: number = this.quizSession.getNumberOfAllQuestions();
    const formattedActualQuizNumberOfAllQuestions: string = `${actualQuizNumberOfAllQuestions}`;

    this.questionInfoTableAllQuestionsNumberEditor.setInnerHTML(formattedActualQuizNumberOfAllQuestions);
  }

  private loadCurrentQuizSessionPageWrongAnswerPenalty() {
    const actualQuizQuestionWrongAnswerPenalty: number = this.quizSession.getCurrentQuestionPenalty();
    const formattedActualQuizQuestionWrongAnswerPenalty: string = `${actualQuizQuestionWrongAnswerPenalty}`;

    this.questionInfoTableTimePenaltyEditor.setInnerHTML(formattedActualQuizQuestionWrongAnswerPenalty);
  }

}

export class CurrentQuizSessionPageEditorStopwatch {

  private static STOPWATCH_TIMEOUT_IM_MS: number = 1000;

  private currentQuizSessionPageUpdater: CurrentQuizSessionPageEditor;
  private counter: number;

  private constructor(currentQuizSessionPageUpdater: CurrentQuizSessionPageEditor) {
    this.currentQuizSessionPageUpdater = currentQuizSessionPageUpdater;
    this.counter = 0;
  }

  public static forUpdaterAndStart(currentQuizSessionPageUpdater: CurrentQuizSessionPageEditor): CurrentQuizSessionPageEditorStopwatch {
    const stopwatch: CurrentQuizSessionPageEditorStopwatch = new CurrentQuizSessionPageEditorStopwatch(currentQuizSessionPageUpdater);
    stopwatch.start();

    return stopwatch;
  }

  public start() {
    this.timer();
  }

  private count() {
    this.counter++;

    this.currentQuizSessionPageUpdater.updateQuizSessionTime(this.counter);
    this.currentQuizSessionPageUpdater.updateCurrentQuestionStopwatch(this.counter);

    this.timer();
  }

  private timer() {
    setTimeout(() => this.count(), CurrentQuizSessionPageEditorStopwatch.STOPWATCH_TIMEOUT_IM_MS);
  }

}
