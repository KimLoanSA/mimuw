import {Utils} from '../utils/utils.js';
import {DocumentEditor} from './documentEditors.js';
import {QuizScore} from '../scoreboards/scoreboard.js';

export class ScoreboardTableEditor {

  private tableElement: HTMLTableElement;
  private documentEditor: DocumentEditor;
  private numberOfRowsToDisplay: number;

  public constructor(document: Document, tableElementId: string, numberOfRowsToDisplay: number) {
    this.documentEditor = DocumentEditor.fromDocument(document);
    this.tableElement = (this.documentEditor.getElement(tableElementId) as HTMLTableElement);
    this.numberOfRowsToDisplay = numberOfRowsToDisplay;
  }

  public addRowsWithScoresInGivenOrder(scoresArray: QuizScore[], className: string) {
    scoresArray
      .slice(0, this.numberOfRowsToDisplay)
      .forEach(score => this.addRowWithScore(score, className));
  }

  private addRowWithScore(score: QuizScore, className: string) {
    const newRow: HTMLTableRowElement = this.tableElement.insertRow();
    const formattedScore: string = Utils.getStringDescriptingTimeInSeconds(score.getScore());

    this.addCellToTableRow(newRow, formattedScore, className);
  }

  private addCellToTableRow(tableRow: HTMLTableRowElement, innerHTML: string, cellClass: string) {
    const newCell = tableRow.insertCell();
    newCell.innerHTML = innerHTML;
    newCell.className = cellClass;
  }
}
