import {DocumentEditor} from './main/editors/documentEditors.js';
import {Properties} from './main/properties/Properties.js';
import {PasswordChangeProperties} from './main/properties/passwordChangeProperties.js';

const documentEditor: DocumentEditor = DocumentEditor.fromDocument(document);
const crsfInput: HTMLInputElement = documentEditor.getElement(Properties.CRSF_INPUT_ID) as HTMLInputElement;

const crsfCookie: string = documentEditor.getCookie(Properties.CRSF_COOKIE_NAME);

crsfInput.value = crsfCookie;

const quizLogoutButton: HTMLButtonElement = documentEditor.getElement(PasswordChangeProperties.PASSWORD_CHANGE_LOGOUT_BUTTON_ID) as HTMLButtonElement;
quizLogoutButton.addEventListener(Properties.CLICK_EVENT_TYPE, quizLogoutButtonClickListener);

function quizLogoutButtonClickListener() {
  location.href = Properties.QUIZ_LOGOUT_HTML_FILE;
}
