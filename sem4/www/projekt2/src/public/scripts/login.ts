import {DocumentEditor} from './main/editors/documentEditors.js';
import {Properties} from './main/properties/Properties.js';

const documentEditor: DocumentEditor = DocumentEditor.fromDocument(document);
const crsfInput: HTMLInputElement = documentEditor.getElement(Properties.CRSF_INPUT_ID) as HTMLInputElement;

const crsfCookie: string = documentEditor.getCookie(Properties.CRSF_COOKIE_NAME);
crsfInput.value = crsfCookie;
