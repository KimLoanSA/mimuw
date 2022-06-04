import { expect } from "chai";
import "mocha";

import {QuestionStatisticsGuard} from "../../main/typeguards/typeguards";

const testValidJson: string = `{
  "isAnswerCorrectFlag": true,
  "timePenalty": 1,
  "timeSpendInSeconds": 1
}`;
runTestForQuestionStatisticsGuard(testValidJson, true, "valid json");

const testInvalidIsAnswerCorrectFieldTypeJson: string = `{
  "isAnswerCorrectFlag": "test",
  "timePenalty": 1,
  "timeSpendInSeconds": 1
}`;
runTestForQuestionStatisticsGuard(testInvalidIsAnswerCorrectFieldTypeJson, false, "invalid isAnswerCorrect field type json");

const testInvalidTimePenaltyFieldTypeJson: string = `{
  "isAnswerCorrectFlag": true,
  "timePenalty": [1],
  "timeSpendInSeconds": 1
}`;
runTestForQuestionStatisticsGuard(testInvalidTimePenaltyFieldTypeJson, false, "invalid timePenalty field type json");

const testInvalidTimeSpendInSecondsFieldTypeJson: string = `{
  "isAnswerCorrectFlag": true,
  "timePenalty": 1,
  "timeSpendInSeconds": "test"
}`;
runTestForQuestionStatisticsGuard(testInvalidTimeSpendInSecondsFieldTypeJson, false, "invalid timeSpendInSeconds field type json");

const testInvalidIsAnswerCorrectFieldNameJson: string = `{
  "isAnswerCrect": true,
  "timePenalty": 1,
  "timeSpendInSeconds": 1
}`;
runTestForQuestionStatisticsGuard(testInvalidIsAnswerCorrectFieldNameJson, false, "invalid isAnswerCorrect field name json");

const testInvalidTimePenaltyFieldNameJson: string = `{
  "isAnswerCorrectFlag": true,
  "timePenaltydsdsa": 1,
  "timeSpendInSeconds": 1
}`;
runTestForQuestionStatisticsGuard(testInvalidTimePenaltyFieldNameJson, false, "invalid timePenalty field name json");

const testInvalidTimeSpendInSecondsFieldNameJson: string = `{
  "isAnswerCorrectFlag": true,
  "timePenalty": 1,
  "timeSpendInSecondrewrews": 1
}`;
runTestForQuestionStatisticsGuard(testInvalidTimeSpendInSecondsFieldNameJson, false, "invalid timeSpendInSeconds field name json");

function runTestForQuestionStatisticsGuard(inputJson: string, expectedValue: boolean, description: string) {
  const parsedTestJson = JSON.parse(inputJson);

  describe("QuestionStatisticsGuard test", () => {
    it(`should return '${expectedValue}' for ${description}`, () => {
      expect(QuestionStatisticsGuard.check(parsedTestJson)).to.equal(expectedValue);
    });
  });
}
