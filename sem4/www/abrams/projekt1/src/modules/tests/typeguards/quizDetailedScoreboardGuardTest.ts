import { expect } from "chai";
import "mocha";

import {QuizDetailedScoreboardGuard} from "../../main/typeguards/typeguards";

const testValidJson: string = `{
  "questionsStatistics": [
    {
      "isAnswerCorrectFlag": true,
      "timePenalty": 1,
      "timeSpendInSeconds": 1
    },
    {
      "isAnswerCorrectFlag": true,
      "timePenalty": 1,
      "timeSpendInSeconds": 1
    }
  ],
  "quizScore": {
    "score": 1
  }
}`;
runTestForQuizDetailedScoreboardGuard(testValidJson, true, "valid json");

const testInvalidQuestionsStatisticsFieldTypeJson: string = `{
  "questionsStatistics": [
    {
      "isAnswerCorrectFlag": true,
      "timePenalty": "test",
      "timeSpendInSeconds": 1
    },
    {
      "isAnswerCorrectFlag": true,
      "timePenalty": 1,
      "timeSpendInSeconds": 1
    }
  ],
  "quizScore": {
    "score": 1
  }
}`;
runTestForQuizDetailedScoreboardGuard(testInvalidQuestionsStatisticsFieldTypeJson, false, "invalid questionsStatistics field type json");

const testInvalidQuizScoreFieldTypeJson: string = `{
  "questionsStatistics": [
    {
      "isAnswerCorrectFlag": true,
      "timePenalty": 1,
      "timeSpendInSeconds": 1
    },
    {
      "isAnswerCorrectFlag": true,
      "timePenalty": 1,
      "timeSpendInSeconds": 1
    }
  ],
  "quizScore": {
    "score": "test"
  }
}`;
runTestForQuizDetailedScoreboardGuard(testInvalidQuizScoreFieldTypeJson, false, "invalid quizScore field type json");

const testInvalidQuestionsStatisticsFieldNameJson: string = `{
  "questionsStatisdss": [
    {
      "isAnswerCorrectFlag": true,
      "timePenalty": 1,
      "timeSpendInSeconds": 1
    },
    {
      "isAnswerCorrectFlag": true,
      "timePenalty": 1,
      "timeSpendInSeconds": 1
    }
  ],
  "quizScore": {
    "score": 1
  }
}`;
runTestForQuizDetailedScoreboardGuard(testInvalidQuestionsStatisticsFieldNameJson, false, "invalid questionsStatistics field name json");

const testInvalidQuizScoreFieldNameJson: string = `{
  "questionsStatistics": [
    {
      "isAnswerCorrectFlag": true,
      "timePenalty": 1,
      "timeSpendInSeconds": 1
    },
    {
      "isAnswerCorrectFlag": true,
      "timePenalty": 1,
      "timeSpendInSeconds": 1
    }
  ],
  "quizS31312re": {
    "score": 1
  }
}`;
runTestForQuizDetailedScoreboardGuard(testInvalidQuizScoreFieldNameJson, false, "invalid quizScore field name json");

function runTestForQuizDetailedScoreboardGuard(inputJson: string, expectedValue: boolean, description: string) {
  const parsedTestJson = JSON.parse(inputJson);

  describe("QuizDetailedScoreboardGuard test", () => {
    it(`should return '${expectedValue}' for ${description}`, () => {
      expect(QuizDetailedScoreboardGuard.check(parsedTestJson)).to.equal(expectedValue);
    });
  });
}
