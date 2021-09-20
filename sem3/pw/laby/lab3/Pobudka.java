import java.util.concurrent.CountDownLatch;

public class Pobudka {

  private static final int PRZYPISANIA = 100000;
  private static final int POBUDKA = 50000;

  private static volatile int licznik = 0;
  private static final CountDownLatch zasuwka = new CountDownLatch(POBUDKA);

  public static void main(final String[] args) {
    Thread główny = Thread.currentThread();

    new Thread(new Runnable() {

      @Override
      public void run() {
        for (int i = 0; i < PRZYPISANIA; ++i) {
          if (Thread.interrupted()) {
            Thread.currentThread().interrupt();
            System.err.println("Pomocniczy przerwany");
            główny.interrupt();
            break;
          }
          licznik = i + 1;
          zasuwka.countDown();
    }
  }

    }).start();

    try {
      zasuwka.await();
      System.out.println(licznik); // >= POBUDKA
    } catch (InterruptedException e) {
      Thread.currentThread().interrupt();
      System.err.println("Główny przerwany");
    }
  }

}
