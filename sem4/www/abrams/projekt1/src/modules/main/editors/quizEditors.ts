import {Utils} from "../utils/utils.js";
import {DocumentEditor} from "./documentEditors.js";
import {QuizScore} from "../scoreboards/scoreboard.js";
import {QuizProperties} from "../properties/quizProperties.js";

export class ScoreboardTableEditor {

  private tableElement: HTMLTableElement;
  private documentEditor: DocumentEditor;
  private numberOfRowsToDisplay: number;

  public constructor(document: Document, tableElementId: string, numberOfRowsToDisplay: number) {
    this.documentEditor = DocumentEditor.fromDocument(document);
    this.tableElement = <HTMLTableElement>this.documentEditor.getElement(tableElementId);
    this.numberOfRowsToDisplay = numberOfRowsToDisplay;
  }

  public addRowsWithScoresInGivenOrder(scoresArray: QuizScore[]) {
    scoresArray
      .slice(0, this.numberOfRowsToDisplay)
      .forEach(score => this.addRowWithScore(score));
  }

  private addRowWithScore(score: QuizScore) {
    const newRow: HTMLTableRowElement = this.tableElement.insertRow();
    const formattedScore: string = Utils.getStringDescriptingTimeInSeconds(score.getScore());

    this.addCellToTableRow(newRow, formattedScore, QuizProperties.QUIZ_SCOREBOARD_TABLE_ELEMENT_CLASS);
  }

  private addCellToTableRow(tableRow: HTMLTableRowElement, innerHTML: string, cellClass: string) {
    const newCell = tableRow.insertCell();
    newCell.innerHTML = innerHTML;
    newCell.className = cellClass;
  }
}
