import {Utils} from "../utils/utils.js";
import {QuizEndingProperties} from "../properties/quizEndingProperties.js";


export class SelectEditor {

  private selectElement: HTMLSelectElement;
  private documentEditor: DocumentEditor;

  public constructor(document: Document, selectElementId: string) {
    this.documentEditor = DocumentEditor.fromDocument(document);
    this.selectElement = <HTMLSelectElement>this.documentEditor.getElement(selectElementId);
  }

  public addOptions(options: string[], optionElementClass: string) {
    options
      .forEach(option =>
          this.createOptionElementAndAddToSelect(option, optionElementClass));
  }

  private createOptionElementAndAddToSelect(optionElementName: string, optionElementClass: string) {
    const optionElement = this.buildOptionElement(optionElementName, optionElementClass);

    this.selectElement.appendChild(optionElement);
  }

  private buildOptionElement(optionElementName: string, optionElementClass: string): HTMLOptionElement {
    const document: Document = this.documentEditor.getDocument();

    return OptionElementBuilder.Builder(document)
      .value(optionElementName)
      .innerHTML(optionElementName)
      .className(optionElementClass)
      .build();
  }

}


export class QuizScoreboardTableEditor {

  private tableElement: HTMLTableElement;
  private documentEditor: DocumentEditor;
  private numberOfRows: number;

  public constructor(document: Document, tableElementId: string) {
    this.documentEditor = DocumentEditor.fromDocument(document);
    this.tableElement = <HTMLTableElement>this.documentEditor.getElement(tableElementId);
    this.numberOfRows = 0;
  }

  public addRowWithAnswerTimeAndPenaltyForQuestion(isAnswerCorrect: boolean, answerTime: number, wrongAnswerPenalty: number) {
    const newRow: HTMLTableRowElement = this.tableElement.insertRow();

    this.addQuestionNumberCellToTableRow(newRow);
    this.addAnswerCorrectnessCellToTableRow(newRow, isAnswerCorrect);
    this.addTimeWithPenaltyIfWrongCellToTAbleRow(newRow, isAnswerCorrect, answerTime, wrongAnswerPenalty);
  }

  private addQuestionNumberCellToTableRow(tableRow: HTMLTableRowElement) {
    this.numberOfRows++;
    const formattedQuestionNumberCell: string = `Pytanie ${this.numberOfRows}:`;

    this.addCellToTableRow(tableRow, formattedQuestionNumberCell, QuizEndingProperties.QUIZ_ENDING_STATS_DETAILS_TABLE_LEFT_ELEMENT_CLASS);
  }

  private addAnswerCorrectnessCellToTableRow(tableRow: HTMLTableRowElement, isAnswerCorrect: boolean) {
    if (isAnswerCorrect) {
      this.addCorrectAnswerCellToTableRow(tableRow);
    } else {
      this.addIncorrectAnswerCellToTableRow(tableRow);
    }
  }

  private addCorrectAnswerCellToTableRow(tableRow: HTMLTableRowElement) {
    this.addCellToTableRow(tableRow, QuizEndingProperties.QUIZ_ENDING_STATS_DETAILS_TABLE_OK_ANSWER,
        QuizEndingProperties.QUIZ_ENDING_STATS_DETAILS_TABLE_MIDDLE_ELEMENT_OK_CLASS);
  }

  private addIncorrectAnswerCellToTableRow(tableRow: HTMLTableRowElement) {
    this.addCellToTableRow(tableRow, QuizEndingProperties.QUIZ_ENDING_STATS_DETAILS_TABLE_WA_ANSWER,
        QuizEndingProperties.QUIZ_ENDING_STATS_DETAILS_TABLE_MIDDLE_ELEMENT_WA_CLASS);
  }

  private addTimeWithPenaltyIfWrongCellToTAbleRow(tableRow: HTMLTableRowElement, isAnswerCorrect: boolean, answerTime: number, wrongAnswerPenalty: number) {
    if (isAnswerCorrect) {
      this.addTimeToTAbleRow(tableRow, answerTime);
    } else {
      this.addTimeWithPenaltyCellToTAbleRow(tableRow, answerTime, wrongAnswerPenalty);
    }
  }

  private addTimeToTAbleRow(tableRow: HTMLTableRowElement, answerTime: number) {
    const formattedTime: string = Utils.getStringDescriptingTimeInSeconds(answerTime);

    this.addCellToTableRow(tableRow, formattedTime, QuizEndingProperties.QUIZ_ENDING_STATS_DETAILS_TABLE_RIGHT_ELEMENT_CLASS);
  }

  private addTimeWithPenaltyCellToTAbleRow(tableRow: HTMLTableRowElement, answerTime: number, wrongAnswerPenalty: number) {
    const formattedTime: string = Utils.getStringDescriptingTimeInSeconds(answerTime);
    const formattedPenaltyTime: string = Utils.getStringDescriptingTimeInSeconds(wrongAnswerPenalty);
    const formattedTimeWithPenalty: string = `${formattedTime} (+ ${formattedPenaltyTime})`;

    this.addCellToTableRow(tableRow, formattedTimeWithPenalty, QuizEndingProperties.QUIZ_ENDING_STATS_DETAILS_TABLE_RIGHT_ELEMENT_CLASS);
  }

  private addCellToTableRow(tableRow: HTMLTableRowElement, innerHTML: string, cellClass: string) {
    const newCell = tableRow.insertCell();
    newCell.innerHTML = innerHTML;
    newCell.className = cellClass;
  }
}


export class HTMLElementEditor {

  private htmlElement: HTMLElement;
  private documentEditor: DocumentEditor;

  public constructor(document: Document, labelElementId: string) {
    this.documentEditor = DocumentEditor.fromDocument(document);
    this.htmlElement = this.documentEditor.getElement(labelElementId);
  }

  public setInnerHTML(innerHTML: string) {
    this.htmlElement.innerHTML = innerHTML;
  }

}

export class DocumentEditor {

  private readonly document: Document;

  private constructor(document: Document) {
    this.document = document;
  }

  public static fromDocument(document: Document): DocumentEditor {
    return new DocumentEditor(document);
  }

  public getElement(elementId: string): HTMLElement {
    const nullableResultElement: HTMLElement | null = this.document.getElementById(elementId);

    return Utils.notNullHTMLElementOrThrowError(nullableResultElement, "invalid element id");
  }

  public getDocument(): Document {
    return this.document;
  }
}

export class OptionElementBuilder {

  private static OPTION_TAG: string = "option";

  private readonly resultOptionElement: HTMLOptionElement;

  private constructor(document: Document) {
    this.resultOptionElement = <HTMLOptionElement>document.createElement(OptionElementBuilder.OPTION_TAG);
  }

  public static Builder(document: Document) {
    return new OptionElementBuilder(document);
  }

  public value(value: string): OptionElementBuilder {
    this.resultOptionElement.value = value;

    return this;
  }

  public innerHTML(value: string): OptionElementBuilder {
    this.resultOptionElement.innerHTML = value;

    return this;
  }

  public className(value: string): OptionElementBuilder {
    this.resultOptionElement.className = value;

    return this;
  }

  public build(): HTMLOptionElement {
    return this.resultOptionElement;
  }

}
