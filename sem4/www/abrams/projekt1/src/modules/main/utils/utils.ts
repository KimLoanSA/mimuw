export class Utils {

  public static getStringOrThrowError(value: string | null, errorMessage: string): string {
    if (value == null) {
      throw new Error(errorMessage);
    }

    return value;
  }

  public static notNullHTMLElementOrThrowError(value: HTMLElement | null, errorMessage: string): HTMLElement {
    if (value == null) {
      throw new Error(errorMessage);
    }

    return value;
  }

  public static getStringDescriptingTimeInSeconds(seconds: number): string {
    if (seconds == 1) {
      return this.getStringDescriptingTimeInSecondsFor1Second();
    } else if (seconds > 1 && seconds < 5) {
      return this.getStringDescriptingTimeInSecondsForGreaterThan1AndLessThan5Seconds(seconds);
    } else {
      return this.getStringDescriptingTimeInSecondsFor0AndGreaterThan4Seconds(seconds);
    }
  }

  private static getStringDescriptingTimeInSecondsFor1Second(): string {
    return "1 sekunda";
  }

  private static getStringDescriptingTimeInSecondsForGreaterThan1AndLessThan5Seconds(seconds: number): string {
    return `${seconds} sekundy`;
  }

  private static getStringDescriptingTimeInSecondsFor0AndGreaterThan4Seconds(seconds: number): string {
    return `${seconds} sekund`;
  }

}
