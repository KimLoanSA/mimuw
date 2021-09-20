import java.util.Arrays;
import java.util.Random;
import java.util.concurrent.atomic.AtomicInteger;

public class LiteryCyfryDoTablicy {

  private static final int WIERSZE = 5;
  private static final int DŁUGOŚĆ = 76;
  private static final int PISANE = DŁUGOŚĆ * WIERSZE / 2;

  private static final int CZEKANIE = 3;

  private static final Random RANDOM = new Random();

  private static final char[] tekst = new char[2 * PISANE];

  private static final char PUSTE = ' ';

  private static final AtomicInteger wolne = new AtomicInteger();

  private static class Piszący implements Runnable {

    private final char pierwszy;
    private final char ostatni;

    public Piszący(char pierwszy, char ostatni) {
      this.pierwszy = pierwszy;
      this.ostatni = ostatni;
    }

    @Override
    public void run() {
      try {
        char c = pierwszy;
        for (int i = 0; i < PISANE; ++i) {
          if (RANDOM.nextInt(CZEKANIE) == 0) {
            Thread.sleep(1);
          }
          int p = wolne.getAndIncrement();
          tekst[p] = c;
          ++c;
          if (c > ostatni) {
            c = pierwszy;
          }
        }
      } catch (InterruptedException e) {
        Thread t = Thread.currentThread();
        t.interrupt();
        System.err.println(t.getName() + " przerwany");
      }
    }

  }

  public static void main(String[] args) {
    Arrays.fill(tekst, PUSTE);
    Thread litery = new Thread(new Piszący('a', 'z'), "Litery");
    Thread cyfry = new Thread(new Piszący('0', '9'), "Cyfry");
    litery.start();
    cyfry.start();
    try {
      litery.join();
      cyfry.join();
      int w = 0;
      for (char c : tekst) {
        System.out.print(c);
        ++w;
        if (w == DŁUGOŚĆ) {
          System.out.println();
          w = 0;
        }
      }
      System.out.println();
    } catch (InterruptedException e) {
      Thread.currentThread().interrupt();
      System.err.println("Główny przerwany");
    }
  }

}
