import {Properties} from "./main/properties/properties.js";
import {QuizProperties} from "./main/properties/quizProperties.js";
import {DocumentEditor, SelectEditor} from "./main/editors/documentEditors.js";
import {Quiz, Quizzes} from "./main/quizzes/quizzes.js";
import {ScoreboardTableEditor} from "./main/editors/quizEditors.js";
import {QuizScore} from "./main/scoreboards/scoreboard.js";
import {IndexedDBClient} from "./main/persistence/indexedDBClient.js";


const documentEditor: DocumentEditor = DocumentEditor.fromDocument(document);

const quizzes: Quizzes = new Quizzes();
const quizzesNamesArray: string[] = quizzes.getQuizzesNames();

const selectEditor: SelectEditor = new SelectEditor(document, QuizProperties.QUIZ_SELECTION_SELECT_ID);
selectEditor.addOptions(quizzesNamesArray, QuizProperties.QUIZ_SELECTION_SELECT_OPTION_ID);

const scoreboardTableEditor: ScoreboardTableEditor =
    new ScoreboardTableEditor(document, QuizProperties.QUIZ_SCOREBOARD_TABLE_ID, QuizProperties.QUIZ_SCOREBOARD_NUMBER_OF_SCOREBOARD_ROWS);

let scoreTableScoresUpdater = (quizScoresArray: QuizScore[]) =>
    scoreboardTableEditor.addRowsWithScoresInGivenOrder(quizScoresArray.sort((a, b) => a.compare(b)));

IndexedDBClient.getAllScoresAndInsertToTableWithSupplier(scoreTableScoresUpdater);


const quizSelectionForm: HTMLFormElement = <HTMLFormElement>documentEditor.getElement(QuizProperties.QUIZ_SELECTION_FORM_ID);
quizSelectionForm.addEventListener(Properties.INPUT_TAG, quizSelectionFormInputListener);

const startQuizButton: HTMLButtonElement = <HTMLButtonElement>documentEditor.getElement(QuizProperties.START_QUIZ_BUTTON_ID);
startQuizButton.addEventListener(Properties.CLICK_EVENT_TYPE, startQuizButtonClickListener);

function quizSelectionFormInputListener(event: any) {
  const chosenQuizName = event.target.value;

  quizzes.updateChosenQuiz(chosenQuizName);
}

function startQuizButtonClickListener() {
  const chosenQuiz: Quiz = quizzes.getChosenQuiz();
  const chosenQuizJson: string = chosenQuiz.toJson();

  sessionStorage.setItem(Properties.QUIZ_SESSION_STORAGE_KEY, chosenQuizJson);
  location.href = Properties.QUIZ_QUESTION_HTML_FILE;
}

