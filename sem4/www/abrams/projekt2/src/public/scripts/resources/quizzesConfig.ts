// tests have to implement interfaces:
export interface QuizQuestionWithAnswerJson {
  question: string,
  answer: number,
  wrongAnswerPenalty: number
}

export interface QuizJson {
  name: string,
  introduction: string,
  questionsWithAnswers: QuizQuestionWithAnswerJson[]
}

