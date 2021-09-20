import java.util.Random;

public class LiteryCyfryWierszamiSwobodnie {

  private static final Random RANDOM = new Random();

  private static final int ZAKRES = 10;
  private static final int MNOŻNIK = 10;

  private static final int N_WIERSZY = 100;
  private static final int W_WIERSZU = 50;

  private static volatile boolean chceLitery = false;
  private static volatile boolean chceCyfry = false;

  private enum Piszący {
    LITERY, CYFRY
  }

  private static volatile Piszący czeka = Piszący.LITERY;

  private static void pracujLokalnie() throws InterruptedException {
    Thread.sleep(MNOŻNIK * RANDOM.nextInt(ZAKRES));
  }

  private static char pisz(char pierwsza, char aktualna, char ostatnia) {
    char c = aktualna;
    for (int j = 0; j < W_WIERSZU; ++j) {
      System.out.print(c);
      ++c;
      if (c > ostatnia) {
        c = pierwsza;
      }
    }
    System.out.println();
    return c;
  }

  private static class Litery implements Runnable {

    private final char PIERWSZA = 'a';
    private final char OSTATNIA = 'z';

    @Override
    public void run() {
      try {
        char c = PIERWSZA;
        for (int i = 0; i < N_WIERSZY; ++i) {
          pracujLokalnie();
          chceLitery = true;
          czeka = Piszący.LITERY;
          while (chceCyfry && (czeka == Piszący.LITERY)) {
            Thread.yield();
          }
          c = pisz(PIERWSZA, c, OSTATNIA);
          chceLitery = false;
        }
      } catch (InterruptedException e) {
        Thread.currentThread().interrupt();
        System.err.println("Litery przerwany");
      }
    }

  }

  private static class Cyfry implements Runnable {

    private final char PIERWSZA = '0';
    private final char OSTATNIA = '9';

    @Override
    public void run() {
      try {
        char c = PIERWSZA;
        for (int i = 0; i < N_WIERSZY; ++i) {
          pracujLokalnie();
          chceCyfry = true;
          czeka = Piszący.CYFRY;
          while (chceLitery && (czeka == Piszący.CYFRY)) {
            Thread.yield();
          }
          c = pisz(PIERWSZA, c, OSTATNIA);
          chceCyfry = false;
        }
      } catch (InterruptedException e) {
        Thread.currentThread().interrupt();
        System.err.println("Cyfry przerwany");
      }
    }

  }

  public static void main(String args[]) {
    Thread litery = new Thread(new Litery());
    Thread cyfry = new Thread(new Cyfry());
    System.out.println("Początek");
    litery.start();
    cyfry.start();
    try {
      litery.join();
      cyfry.join();
      System.out.println("Koniec");
    } catch (InterruptedException e) {
      System.err.println("Główny przerwany");
      Thread.currentThread().interrupt();
      litery.interrupt();
      cyfry.interrupt();
    }
  }

}