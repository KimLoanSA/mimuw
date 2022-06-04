import {QuestionStatistics, QuizDetailedScoreboard} from '../scoreboards/scoreboard.js';
import {HTMLElementEditor, QuizScoreboardTableEditor} from './documentEditors.js';
import {QuizEndingProperties} from '../properties/quizEndingProperties.js';
import {Utils} from '../utils/utils.js';

export class QuizEndingPageEditor {

  private detailedScoreboard: QuizDetailedScoreboard;

  private quizStatsAnswerEditor: HTMLElementEditor;
  private quizStatsResultEditor: HTMLElementEditor;
  private quizDetailsStatsTableEditor: QuizScoreboardTableEditor;

  public constructor(document: Document, detailedScoreboard: QuizDetailedScoreboard) {
    this.detailedScoreboard = detailedScoreboard;

    this.quizStatsAnswerEditor = new HTMLElementEditor(document, QuizEndingProperties.QUIZ_ENDING_STATS_TABLE_ANSWERS_ID);
    this.quizStatsResultEditor = new HTMLElementEditor(document, QuizEndingProperties.QUIZ_ENDING_STATS_TABLE_RESULT_ID);
    this.quizDetailsStatsTableEditor = new QuizScoreboardTableEditor(document, QuizEndingProperties.QUIZ_ENDING_STATS_DETAILS_TABLE_ID);
  }

  public loadPage() {
    this.loadPageQuizStatsAnswer();
    this.loadPageQuizStatsResult();
    this.loadPageDetailsStatsTable();
  }

  private loadPageQuizStatsAnswer() {
    const quizStatsNumberOfCorrectAnswers: number = this.detailedScoreboard.getNumberOfCorrectsAnswers();
    const quizStatsNumberOfAnswers: number = this.detailedScoreboard.getNumberOfAnswers();
    const formattedQuizStatsAnswer: string = `${quizStatsNumberOfCorrectAnswers} / ${quizStatsNumberOfAnswers}`;

    this.quizStatsAnswerEditor.setInnerHTML(formattedQuizStatsAnswer);
  }

  private loadPageQuizStatsResult() {
    const quizStatsResult: number = this.detailedScoreboard.getNumericQuizScore();
    const formattedQuizStatsResult: string = Utils.getStringDescriptingTimeInSeconds(quizStatsResult);

    this.quizStatsResultEditor.setInnerHTML(formattedQuizStatsResult);
  }

  private loadPageDetailsStatsTable() {
    this.detailedScoreboard
        .getQuestionsStatistics()
        .forEach(questionStatistics => this.loadPageDetailsStatsTableRow(questionStatistics));
  }

  private loadPageDetailsStatsTableRow(questionStatistics: QuestionStatistics) {
    const isAnswerCorrect: boolean = questionStatistics.isAnswerCorrect();
    const answerTime: number = questionStatistics.getAnswerTime();
    const timePenalty: number = questionStatistics.getTimePenalty();
    const correctAnswer: number = questionStatistics.getCorrectAnswer();
    const averageAnswerTime: number = questionStatistics.getAverageAnswerTime();

    this.quizDetailsStatsTableEditor
      .addRowWithAnswerTimeAndPenaltyForQuestion(isAnswerCorrect, correctAnswer, answerTime, timePenalty, averageAnswerTime);
  }

}
