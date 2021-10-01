import {Properties} from "./main/properties/properties.js";
import {QuizEndingProperties} from "./main/properties/quizEndingProperties.js";
import {Utils} from "./main/utils/utils.js";
import {QuizDetailedScoreboard} from "./main/scoreboards/scoreboard.js";
import {QuizEndingPageEditor} from "./main/editors/quizEndingEditors.js";
import {IndexedDBClient} from "./main/persistence/indexedDBClient.js";
import {DocumentEditor} from "./main/editors/documentEditors.js";


const documentEditor: DocumentEditor = DocumentEditor.fromDocument(document);

const nullableDetailedScoreboardJson: string | null = sessionStorage.getItem(Properties.QUIZ_DETAILED_SCOREBOARD_SESSION_STORAGE_KEY);
const detailedScoreboardJson: string = Utils.getStringOrThrowError(nullableDetailedScoreboardJson, "invalid session storage key");

const detailedScoreboard: QuizDetailedScoreboard = QuizDetailedScoreboard.fromJson(detailedScoreboardJson);

const quizEndingPageUpdater: QuizEndingPageEditor = new QuizEndingPageEditor(document, detailedScoreboard);
quizEndingPageUpdater.loadPage();

IndexedDBClient.saveScore(detailedScoreboard.getQuizScore());


const savingSimpleScoreButton: HTMLButtonElement = <HTMLButtonElement>documentEditor.getElement(QuizEndingProperties.QUIZ_ENDING_SAVING_BUTTON_SIMPLE_SCORE);
savingSimpleScoreButton.addEventListener(Properties.CLICK_EVENT_TYPE, savingSimpleScoreButtonClickListener);

const savingDetailedScoreButton: HTMLButtonElement = <HTMLButtonElement>documentEditor.getElement(QuizEndingProperties.QUIZ_ENDING_SAVING_BUTTON_DETAILED_SCORE);
savingDetailedScoreButton.addEventListener(Properties.CLICK_EVENT_TYPE, savingDetailedScoreButtonClickListener);

function savingSimpleScoreButtonClickListener() {
  location.href = Properties.QUIZ_HTML_FILE;
}

function savingDetailedScoreButtonClickListener() {
  IndexedDBClient.saveDetailedScore(detailedScoreboard);

  location.href = Properties.QUIZ_HTML_FILE;
}
