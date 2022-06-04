import { expect } from "chai";
import "mocha";

import {QuizScoreGuard} from "../../main/typeguards/typeguards";

const testValidJson: string = `{
  "score": 1
}`;
runTestForQuizScoreGuard(testValidJson, true, "valid json");

const testInvalidScoreFieldTypeJson: string = `{
  "score": [1]
}`;
runTestForQuizScoreGuard(testInvalidScoreFieldTypeJson, false, "invalid score field type json");

const testInvalidScoreFieldNameJson: string = `{
  "scor": 1
}`;
runTestForQuizScoreGuard(testInvalidScoreFieldNameJson, false, "invalid score field name json");

function runTestForQuizScoreGuard(inputJson: string, expectedValue: boolean, description: string) {
  const parsedTestJson = JSON.parse(inputJson);

  describe("QuizScoreGuard test", () => {
    it(`should return '${expectedValue}' for ${description}`, () => {
      expect(QuizScoreGuard.check(parsedTestJson)).to.equal(expectedValue);
    });
  });
}
