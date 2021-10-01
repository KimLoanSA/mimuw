import {Properties} from './main/properties/properties.js';
import {QuizProperties} from './main/properties/quizProperties.js';
import {DocumentEditor, SelectEditor} from './main/editors/documentEditors.js';
import {ScoreboardTableEditor} from './main/editors/quizEditors.js';
import {QuizScore} from './main/scoreboards/scoreboard.js';
import {HttpClient} from './main/httpclient/httpClient.js';


const documentEditor: DocumentEditor = DocumentEditor.fromDocument(document);

const httpClient: HttpClient = new HttpClient();

const selectEditor: SelectEditor =
    new SelectEditor(document, QuizProperties.QUIZ_SELECTION_SELECT_ID);
const scoreboardTableEditor: ScoreboardTableEditor =
    new ScoreboardTableEditor(document, QuizProperties.QUIZ_SCOREBOARD_TABLE_ID, QuizProperties.QUIZ_SCOREBOARD_NUMBER_OF_SCOREBOARD_ROWS);
const statsSelectEditor: SelectEditor =
    new SelectEditor(document, QuizProperties.QUIZ_STATS_SELECT_ID);

let chosenQuizName: string = '';
let chosenQuizStatsQuizName: string = '';

httpClient.getQuizzesNamesList()
  .then(quizzesNamesArray => updateChosenQuizAndAddOptions(quizzesNamesArray));

function updateChosenQuizAndAddOptions(quizzesNames: string[]) {
  chosenQuizName = quizzesNames[0] !== undefined ? quizzesNames[0] : '';
  selectEditor.addOptions(quizzesNames, QuizProperties.QUIZ_SELECTION_SELECT_OPTION_ID)
}

httpClient.getSolvedQuizzesNamesList()
  .then(quizzesNamesArray => updateChosenQuizStatsAndAddOptions(quizzesNamesArray));

function updateChosenQuizStatsAndAddOptions(quizzesNames: string[]) {
  chosenQuizStatsQuizName = quizzesNames[0] !== undefined ? quizzesNames[0] : '';
  statsSelectEditor.addOptions(quizzesNames, QuizProperties.QUIZ_SELECTION_SELECT_OPTION_ID)
}

httpClient.getTopScores()
  .then(result => mapScoresAndAddRows(result));

function mapScoresAndAddRows(scores: number[]) {
  const mappedAndSortedScores: QuizScore[] = scores
    .map(o => new QuizScore(o))
    .sort((a, b) => a.compare(b));

  scoreboardTableEditor.addRowsWithScoresInGivenOrder(mappedAndSortedScores, QuizProperties.QUIZ_SCOREBOARD_TABLE_ELEMENT_CLASS);
}

const quizSelectionForm: HTMLFormElement = documentEditor.getElement(QuizProperties.QUIZ_SELECTION_FORM_ID) as HTMLFormElement;
quizSelectionForm.addEventListener(Properties.INPUT_TAG, quizSelectionFormInputListener);

const startQuizButton: HTMLButtonElement = documentEditor.getElement(QuizProperties.START_QUIZ_BUTTON_ID) as HTMLButtonElement;
startQuizButton.addEventListener(Properties.CLICK_EVENT_TYPE, startQuizButtonClickListener);

function quizSelectionFormInputListener(event: any) {
  chosenQuizName = event.target.value;
}

function startQuizButtonClickListener() {
  httpClient.getQuizWithName(chosenQuizName)
    .then(quiz => setQuizAndRedirect(quiz));
}

function setQuizAndRedirect(quiz: string) {
  sessionStorage.setItem(Properties.QUIZ_SESSION_STORAGE_KEY, quiz);
  location.href = Properties.QUIZ_QUESTION_HTML_FILE;
}


const quizStatsSelectionForm: HTMLFormElement = documentEditor.getElement(QuizProperties.QUIZ_STATS_SELECTION_FORM_ID) as HTMLFormElement;
quizStatsSelectionForm.addEventListener(Properties.INPUT_TAG, quizStatsSelectionFormInputListener);

const quizStatsButton: HTMLButtonElement = documentEditor.getElement(QuizProperties.QUIZ_STATS_BUTTON_ID) as HTMLButtonElement;
quizStatsButton.addEventListener(Properties.CLICK_EVENT_TYPE, quizStatsButtonClickListener);

const quizLogoutButton: HTMLButtonElement = documentEditor.getElement(QuizProperties.QUIZ_LOGOUT_BUTTON_ID) as HTMLButtonElement;
quizLogoutButton.addEventListener(Properties.CLICK_EVENT_TYPE, quizLogoutButtonClickListener);

const quizPasswordChangeButton: HTMLButtonElement = documentEditor.getElement(QuizProperties.QUIZ_PASSWORD_CHANGE_BUTTON_ID) as HTMLButtonElement;
quizPasswordChangeButton.addEventListener(Properties.CLICK_EVENT_TYPE, quizPasswordChangeButtonClickListener);

function quizStatsSelectionFormInputListener(event: any) {
  chosenQuizStatsQuizName = event.target.value;
}

function quizStatsButtonClickListener() {
  sessionStorage.setItem(Properties.QUIZ_NAME_SESSION_STORAGE_KEY, chosenQuizStatsQuizName);
  location.href = Properties.QUIZ_ENDING_HTML_FILE;
}

function quizLogoutButtonClickListener() {
  location.href = Properties.QUIZ_LOGOUT_HTML_FILE;
}

function quizPasswordChangeButtonClickListener() {
  location.href = Properties.QUIZ_PASSWORD_CHANGE_HTML_FILE;
}
