import {Properties} from './main/properties/properties.js';
import {QuizEndingProperties} from './main/properties/quizEndingProperties.js';
import {QuizDetailedScoreboard, QuizScore} from './main/scoreboards/scoreboard.js';
import {QuizEndingPageEditor} from './main/editors/quizEndingEditors.js';
import {DocumentEditor} from './main/editors/documentEditors.js';
import {HttpClient} from './main/httpclient/httpClient.js';
import {ScoreboardTableEditor} from './main/editors/quizEditors.js';
import {Utils} from './main/utils/utils.js';

const documentEditor: DocumentEditor = DocumentEditor.fromDocument(document);
const httpClient: HttpClient = new HttpClient();

const scoreboardTableEditor: ScoreboardTableEditor =
    new ScoreboardTableEditor(document, QuizEndingProperties.QUIZ_ENDING_SCOREBOARD_TABLE_ID, QuizEndingProperties.QUIZ_ENDING_SCOREBOARD_NUMBER_OF_SCOREBOARD_ROWS);

const nullableQuizName: string | null = sessionStorage.getItem(Properties.QUIZ_NAME_SESSION_STORAGE_KEY);
const quizName: string = Utils.getStringOrThrowError(nullableQuizName, 'invalid session storage key');

httpClient.getQuizStatistics(quizName)
.then(o => {
  const detailedScoreboard: QuizDetailedScoreboard = QuizDetailedScoreboard.fromJson(o);
  const quizEndingPageUpdater: QuizEndingPageEditor = new QuizEndingPageEditor(document, detailedScoreboard);
  quizEndingPageUpdater.loadPage();
});

httpClient.getQuizTopScores(quizName)
  .then( o => mapScoresAndAddRows(o));

function mapScoresAndAddRows(scores: number[]) {
  const mappedAndSortedScores: QuizScore[] = scores
  .map(o => new QuizScore(o))
  .sort((a, b) => a.compare(b))
  .slice(0, 5);

  scoreboardTableEditor.addRowsWithScoresInGivenOrder(mappedAndSortedScores, QuizEndingProperties.QUIZ_ENDING_SCOREBOARD_TABLE_CLASS);
}


const returnButton: HTMLButtonElement = documentEditor.getElement(QuizEndingProperties.QUIZ_ENDING_RETURN_BUTTON) as HTMLButtonElement;
returnButton.addEventListener(Properties.CLICK_EVENT_TYPE, returnButtonClickListener);

const quizLogoutButton: HTMLButtonElement = documentEditor.getElement(QuizEndingProperties.QUIZ_ENDING_LOGOUT_BUTTON_ID) as HTMLButtonElement;
quizLogoutButton.addEventListener(Properties.CLICK_EVENT_TYPE, quizLogoutButtonClickListener);

function returnButtonClickListener() {
  location.href = Properties.QUIZ_HTML_FILE;
}

function quizLogoutButtonClickListener() {
  location.href = Properties.QUIZ_LOGOUT_HTML_FILE;
}
