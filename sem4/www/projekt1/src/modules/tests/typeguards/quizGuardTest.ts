import { expect } from "chai";
import "mocha";

import {QuizGuard} from "../../main/typeguards/typeguards";

const testValidJson: string = `{
  "name": "test",
  "introduction": "test test",
  "questionsWithAnswers": [
    {
      "question": "test1",
      "answer": 1,
      "wrongAnswerPenalty": 1
    },
    {
      "question": "test2",
      "answer": 2,
      "wrongAnswerPenalty": 3
    }
  ]
}`;
runTestForQuizGuard(testValidJson, true, "valid json");

const testInvalidNameTypeJson: string = `{
  "name": 1,
  "introduction": "test test",
  "questionsWithAnswers": [
    {
      "question": "test1",
      "answer": 1,
      "wrongAnswerPenalty": 1
    },
    {
      "question": "test2",
      "answer": 2,
      "wrongAnswerPenalty": 5
    }
  ]
}`;
runTestForQuizGuard(testInvalidNameTypeJson, false, "invalid name field type json");

const testinvalidIntroductionTypeJson: string = `{
  "name": "test",
  "introduction": 1,
  "questionsWithAnswers": [
    {
      "question": "test1",
      "answer": 1,
      "wrongAnswerPenalty": 1
    },
    {
      "question": "test2",
      "answer": 2,
      "wrongAnswerPenalty": 13
    }
  ]
}`;
runTestForQuizGuard(testinvalidIntroductionTypeJson, false, "invalid introduction field type json");

const testInvalidQuestionsWithAnswersTypeJson: string = `{
  "name": "test",
  "introduction": "test test",
  "questionsWithAnswers": [
    {
      "question": "test1",
      "answer": "test",
      "wrongAnswerPenalty": 1
    },
    {
      "question": "test2",
      "answer": 2,
      "wrongAnswerPenalty": 1
    }
  ]
}`;
runTestForQuizGuard(testInvalidQuestionsWithAnswersTypeJson, false, "invalid questionsWithAnswers field type json");

const testInvalidNameFieldNameJson: string = `{
  "naddme": "test",
  "introduction": "test test",
  "questionsWithAnswers": [
    {
      "question": "test1",
      "answer": 1,
      "wrongAnswerPenalty": 1
    },
    {
      "question": "test2",
      "answer": 2,
      "wrongAnswerPenalty": 1
    }
  ]
}`;
runTestForQuizGuard(testInvalidNameFieldNameJson, false, "invalid name field name json");

const testInvalidIntroductionFieldNameJson: string = `{
  "name": "test",
  "introuction": "test test",
  "questionsWithAnswers": [
    {
      "question": "test1",
      "answer": 1,
      "wrongAnswerPenalty": 1
    },
    {
      "question": "test2",
      "answer": 2,
      "wrongAnswerPenalty": 1
    }
  ]
}`;
runTestForQuizGuard(testInvalidIntroductionFieldNameJson, false, "invalid introduction field name json");

const testInvalidQuestionsWithAnswersFieldNameJson: string = `{
  "name": "test",
  "introduction": "test test",
  "questdffsdnsWithAnswers": [
    {
      "question": "test1",
      "answer": 1,
      "wrongAnswerPenalty": 1
    },
    {
      "question": "test2",
      "answer": 2,
      "wrongAnswerPenalty": 1
    }
  ]
}`;
runTestForQuizGuard(testInvalidQuestionsWithAnswersFieldNameJson, false, "invalid questionsWithAnswers field name json");

function runTestForQuizGuard(inputJson: string, expectedValue: boolean, description: string) {
  const parsedTestJson = JSON.parse(inputJson);

  describe("QuizGuard test", () => {
    it(`should return '${expectedValue}' for ${description}`, () => {
      expect(QuizGuard.check(parsedTestJson)).to.equal(expectedValue);
    });
  });
}
