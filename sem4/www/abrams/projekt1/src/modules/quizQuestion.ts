import {Properties} from "./main/properties/properties.js";
import {Quiz} from "./main/quizzes/quizzes.js";
import {Utils} from "./main/utils/utils.js";
import {DocumentEditor} from "./main/editors/documentEditors.js";
import {QuizSession} from "./main/quizzes/quizSession.js";
import {CurrentQuizSessionPageEditor, CurrentQuizSessionPageEditorStopwatch} from "./main/editors/quizQuestionEditors.js";
import {QuizQuestionProperties} from "./main/properties/quizQuestionProperties.js";
import {QuizDetailedScoreboard} from "./main/scoreboards/scoreboard.js";


const documentEditor: DocumentEditor = DocumentEditor.fromDocument(document);

const nullableQuizJson: string | null = sessionStorage.getItem(Properties.QUIZ_SESSION_STORAGE_KEY);
const quizJson: string = Utils.getStringOrThrowError(nullableQuizJson, "invalid session storage key");
const quiz: Quiz = Quiz.fromJson(quizJson);
const quizSession: QuizSession = QuizSession.startWithQuiz(quiz);

const currentQuizSessionPageUpdater: CurrentQuizSessionPageEditor = new CurrentQuizSessionPageEditor(document, quizSession);

CurrentQuizSessionPageEditorStopwatch.forUpdaterAndStart(currentQuizSessionPageUpdater);


const answerInput: HTMLInputElement = <HTMLInputElement>documentEditor.getElement(QuizQuestionProperties.QUIZ_QUESTION_ANSWER_INPUT_ID);
answerInput.addEventListener(Properties.INPUT_EVENT_TYPE, answerInputListener);
answerInput.placeholder = QuizQuestionProperties.QUIZ_QUESTION_ANSWER_INPUT_PLACEHOLDER;

const cancelButton: HTMLButtonElement = <HTMLButtonElement>documentEditor.getElement(QuizQuestionProperties.QUIZ_QUESTION_CANCEL_BUTTON_ID);
cancelButton.addEventListener(Properties.CLICK_EVENT_TYPE, cancelButtonClickListener);

const navigationBackButton: HTMLButtonElement = <HTMLButtonElement>documentEditor.getElement(QuizQuestionProperties.QUIZ_QUESTION_NAVIGATION_BACK_BUTTON_ID);
navigationBackButton.addEventListener(Properties.CLICK_EVENT_TYPE, navigationBackButtonClickListener);

const navigationStopButton: HTMLButtonElement = <HTMLButtonElement>documentEditor.getElement(QuizQuestionProperties.QUIZ_QUESTION_NAVIGATION_STOP_BUTTON_ID);
navigationStopButton.addEventListener(Properties.CLICK_EVENT_TYPE, navigationStopButtonClickListener);

const navigationNextButton: HTMLButtonElement = <HTMLButtonElement>documentEditor.getElement(QuizQuestionProperties.QUIZ_QUESTION_NAVIGATION_NEXT_BUTTON_ID);
navigationNextButton.addEventListener(Properties.CLICK_EVENT_TYPE, navigationNextButtonClickListener);

updateButtonsVisibilityIfNeededAndUpdatePage();

function answerInputListener(event: any) {
  const insertedValue = event.target.value;

  if (insertedValue.length > 0) {
    updateUserAnswer(insertedValue);
  } else {
    removeUserAnswer();
  }

  updateButtonsVisibilityIfNeededAndUpdatePage();
}

function updateUserAnswer(userAnswer: string) {
  const parsedUserAnswer = Number(userAnswer);

  quizSession.updateUserAnswerForCurrentQuestion(parsedUserAnswer);
}

function removeUserAnswer() {
  quizSession.removeUserAnswerForCurrentQuestion();
}

function cancelButtonClickListener() {
  sessionStorage.removeItem(Properties.QUIZ_SESSION_STORAGE_KEY);

  location.href = Properties.QUIZ_HTML_FILE;
}

function navigationBackButtonClickListener() {
  quizSession.loadPreviousQuestion();
  updateAnswerInputValue();
  updateButtonsVisibilityIfNeededAndUpdatePage();
}

function navigationStopButtonClickListener() {
  const quizDetaildedScoreboard: QuizDetailedScoreboard = quizSession.getDetailedScoreboard();
  const quizDetaildedScoreboardJson: string = quizDetaildedScoreboard.toJson();

  sessionStorage.removeItem(Properties.QUIZ_SESSION_STORAGE_KEY);
  sessionStorage.setItem(Properties.QUIZ_DETAILED_SCOREBOARD_SESSION_STORAGE_KEY, quizDetaildedScoreboardJson);

  location.href = Properties.QUIZ_ENDING_HTML_FILE;
}

function navigationNextButtonClickListener() {
  quizSession.loadNextQuestion();
  updateAnswerInputValue();
  updateButtonsVisibilityIfNeededAndUpdatePage();
}

function updateButtonsVisibilityIfNeededAndUpdatePage() {
  updateButtonsVisibilityIfNeeded();
  currentQuizSessionPageUpdater.loadCurrentQuizSessionPage();
}

function updateButtonsVisibilityIfNeeded() {
  updateNavigationBackButtonVisibilityIfNeeded();
  updateNavigationNextButtonVisibilityIfNeeded();
  updateNavigationStopButtonVisibilityIfNeeded();
}

function updateNavigationBackButtonVisibilityIfNeeded() {
  if (quizSession.hasPreviousQuestion()) {
    navigationBackButton.removeAttribute(Properties.DISABLED_ATTRIBUTE);
  } else {
    navigationBackButton.setAttribute(Properties.DISABLED_ATTRIBUTE, Properties.TRUE);
  }
}

function updateNavigationNextButtonVisibilityIfNeeded() {
  if (quizSession.hasNextQuestion()) {
    navigationNextButton.removeAttribute(Properties.DISABLED_ATTRIBUTE);
  } else {
    navigationNextButton.setAttribute(Properties.DISABLED_ATTRIBUTE, Properties.TRUE);
  }
}

function updateNavigationStopButtonVisibilityIfNeeded() {
  if (quizSession.areAllQuestionsAnswered()) {
    navigationStopButton.removeAttribute(Properties.DISABLED_ATTRIBUTE);
  } else {
    navigationStopButton.setAttribute(Properties.DISABLED_ATTRIBUTE, Properties.TRUE);
  }
}

function updateAnswerInputValue() {
  answerInput.value = getUserAnswerForCurrentQuestionOrEmpty();
}

function getUserAnswerForCurrentQuestionOrEmpty(): string {
  if (quizSession.doesUserAnsweredForCurrentQuestion()) {
    return String(quizSession.getUserAnswerForCurrentQuestion());
  } else {
    return "";
  }
}
