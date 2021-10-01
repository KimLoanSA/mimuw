import {QuizJson, QuizQuestionWithAnswerJson} from '../../resources/quizzesConfig.js';
import {QuestionStatistics, QuizDetailedScoreboard, QuizScore} from "../scoreboards/scoreboard";

export class TypeGuardsUtils {

  public static STRING_TYPE: string = "string";
  public static NUMBER_TYPE: string = "number";
  public static BOOLEAN_TYPE: string = "boolean";

  public static doesObjectContainsFields(object: any, fields: string[]): boolean {
    for (let field of fields) {
      if (!(field in object)) {
        return false;
      }
    }

    return true;
  }

  public static isFieldAnArray(field: any): boolean {
    return field instanceof Array
  }

  public static isFieldOfType(field: any, requiredFieldType: string): boolean {
    return typeof field == requiredFieldType
  }

}


export class QuizGuard {

  private static OBJECT_FIELDS: string[] = ["name", "introduction", "questionsWithAnswers"];

  public static check(object: any): object is QuizJson {
    return TypeGuardsUtils.doesObjectContainsFields(object, this.OBJECT_FIELDS)
        && TypeGuardsUtils.isFieldOfType(object.name, TypeGuardsUtils.STRING_TYPE)
        && TypeGuardsUtils.isFieldOfType(object.introduction, TypeGuardsUtils.STRING_TYPE)
        && this.isFieldAnArrayOfQuestionsWithAnswers(object.questionsWithAnswers);
  }

  private static isFieldAnArrayOfQuestionsWithAnswers(field: any): boolean {
    return TypeGuardsUtils.isFieldAnArray(field)
      && this.areFieldsInArrayInstancesOfQuestionsWithAnswers(field);
  }

  private static areFieldsInArrayInstancesOfQuestionsWithAnswers(fields: any[]): boolean {
    return fields
      .every(field => this.isFieldInstanceOfQuestionsWithAnswers(field));
  }

  private static isFieldInstanceOfQuestionsWithAnswers(field: any): boolean {
    return QuizQuestionWithAnswerGuard.check(field);
  }

}


export class QuizQuestionWithAnswerGuard {

  private static OBJECT_FIELDS: string[] = ["question", "answer", "wrongAnswerPenalty"];

  public static check(object: any): object is QuizQuestionWithAnswerJson {
    return TypeGuardsUtils.doesObjectContainsFields(object, this.OBJECT_FIELDS)
        && TypeGuardsUtils.isFieldOfType(object.question, TypeGuardsUtils.STRING_TYPE)
        && TypeGuardsUtils.isFieldOfType(object.answer, TypeGuardsUtils.NUMBER_TYPE)
        && TypeGuardsUtils.isFieldOfType(object.wrongAnswerPenalty, TypeGuardsUtils.NUMBER_TYPE);
  }

}


export class QuizDetailedScoreboardGuard {

  private static OBJECT_FIELDS: string[] = ["questionsStatistics", "quizScore"];

  public static check(object: any): object is QuizDetailedScoreboard {
    return TypeGuardsUtils.doesObjectContainsFields(object, this.OBJECT_FIELDS)
        && this.isFieldAnArrayOfQuestionStatistics(object.questionsStatistics)
        && this.isFieldInstanceOfQuizScore(object.quizScore);
  }

  private static isFieldAnArrayOfQuestionStatistics(field: any): boolean {
    return TypeGuardsUtils.isFieldAnArray(field)
        && this.areFieldsInArrayInstancesOfQuestionStatistics(field);
  }

  private static areFieldsInArrayInstancesOfQuestionStatistics(fields: any[]): boolean {
    return fields
      .every(field => this.isFieldInstanceOfQuestionStatistics(field));
  }

  private static isFieldInstanceOfQuestionStatistics(field: any): boolean {
    return QuestionStatisticsGuard.check(field);
  }

  private static isFieldInstanceOfQuizScore(field: any): boolean {
    return QuizScoreGuard.check(field);
  }

}


export class QuizScoreGuard {

  private static OBJECT_FIELDS: string[] = ["score"];

  public static check(object: any): object is QuizScore {
    return TypeGuardsUtils.doesObjectContainsFields(object, this.OBJECT_FIELDS)
        && TypeGuardsUtils.isFieldOfType(object.score, TypeGuardsUtils.NUMBER_TYPE)
  }

}


export class QuestionStatisticsGuard {

  private static OBJECT_FIELDS: string[] = ["isAnswerCorrectFlag", "timePenalty", "timeSpendInSeconds"];

  public static check(object: any): object is QuestionStatistics {
    return TypeGuardsUtils.doesObjectContainsFields(object, this.OBJECT_FIELDS)
        && TypeGuardsUtils.isFieldOfType(object.isAnswerCorrectFlag, TypeGuardsUtils.BOOLEAN_TYPE)
        && TypeGuardsUtils.isFieldOfType(object.timePenalty, TypeGuardsUtils.NUMBER_TYPE)
        && TypeGuardsUtils.isFieldOfType(object.timeSpendInSeconds, TypeGuardsUtils.NUMBER_TYPE);
  }

}


