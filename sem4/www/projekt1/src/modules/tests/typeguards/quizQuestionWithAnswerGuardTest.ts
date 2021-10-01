import { expect } from "chai";
import "mocha";

import {QuizQuestionWithAnswerGuard} from "../../main/typeguards/typeguards";

const testValidJson: string = `{
  "question": "test",
  "answer": 1,
  "wrongAnswerPenalty": 1
}`;
runTestForQuizQuestionWithAnswerGuard(testValidJson, true, "valid json");

const testInvalidQuestionTypeJson: string = `{
  "question": 1,
  "answer": 1,
  "wrongAnswerPenalty": 1
}`;
runTestForQuizQuestionWithAnswerGuard(testInvalidQuestionTypeJson, false, "invalid question type json");

const testInvalidAnswerTypeJson: string = `{
  "question": "test",
  "answer": [1],
  "wrongAnswerPenalty": 1
}`;
runTestForQuizQuestionWithAnswerGuard(testInvalidAnswerTypeJson, false, "invalid answer type json");

const testInvalidwrongAnswerPenaltyTypeJson: string = `{
  "question": "test",
  "answer": 1,
  "wrongAnswerPenalty": [1]
}`;
runTestForQuizQuestionWithAnswerGuard(testInvalidwrongAnswerPenaltyTypeJson, false, "invalid wrongAnswerPenalty type json");

const testInvalidQuestionNameJson: string = `{
  "uestion": "test",
  "answer": 1,
  "wrongAnswerPenalty": 1
}`;
runTestForQuizQuestionWithAnswerGuard(testInvalidQuestionNameJson, false, "invalid question field name json");

const testInvalidAnswerNameJson: string = `{
  "question": "test",
  "anwer": 1,
  "wrongAnswerPenalty": 1
}`;
runTestForQuizQuestionWithAnswerGuard(testInvalidAnswerNameJson, false, "invalid answer field name json");

const testInvalidwrongAnswerPenaltyNameJson: string = `{
  "question": "test",
  "answer": 1,
  "wrongnswerPenalty": 1
}`;
runTestForQuizQuestionWithAnswerGuard(testInvalidwrongAnswerPenaltyNameJson, false, "invalid wrongAnswerPenalty field name json");

const testInvalidNumberOfFieldsJson: string = `{
  "answer": 1,
  "wrongAnswerPenalty": 1
}`;
runTestForQuizQuestionWithAnswerGuard(testInvalidNumberOfFieldsJson, false, "invalid number of fields json");

function runTestForQuizQuestionWithAnswerGuard(inputJson: string, expectedValue: boolean, description: string) {
  const parsedTestJson = JSON.parse(inputJson);

  describe("QuizQuestionWithAnswerGuard test", () => {
    it(`should return '${expectedValue}' for ${description}`, () => {
      expect(QuizQuestionWithAnswerGuard.check(parsedTestJson)).to.equal(expectedValue);
    });
  });
}
