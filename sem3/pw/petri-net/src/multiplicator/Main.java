package multiplicator;

import java.util.Scanner;

public class Main {

  private final static Boolean FAIR = true;
  private final static Scanner scanner = new Scanner(System.in);

  public static void main(String[] args) {
    readNumbersAndCreateMultiplicator();
  }

  private static void readNumbersAndCreateMultiplicator() {

    int a = scanner.nextInt();
    int b = scanner.nextInt();

    new Multiplicator(a, b, FAIR).run();
  }

}
