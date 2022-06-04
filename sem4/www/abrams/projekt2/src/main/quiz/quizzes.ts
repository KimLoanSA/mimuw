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


export class Quiz {
  private readonly quizJson: QuizJson;

  private constructor(quizJson: QuizJson) {
    this.quizJson = quizJson;
  }

  public static fromJson(quizJson: string): Quiz {
    const parsedQuizJson = JSON.parse(quizJson);

    return new Quiz(parsedQuizJson);
  }

  public toJson(): string {
    return JSON.stringify(this.quizJson);
  }

  public getName(): string {
    return this.quizJson.name;
  }

  public hasName(name: string): boolean {
    return this.getName() === name;
  }

  public getQuestionsWithAnswers(): QuizQuestionWithAnswerJson[] {
    return this.quizJson
        .questionsWithAnswers;
  }

}
