import java.util.concurrent.Semaphore;

public class DzielonyLicznik {

  private static final int ITERACJE = 10000000;

  private static int licznik = 0;

  private static Semaphore mutex = new Semaphore(1);

  private static class Zwiększający implements Runnable {

    private static void sprawdźPrzerwanie() throws InterruptedException {
      if (Thread.interrupted()) {
        throw new InterruptedException();
      }
    }

    private static void sekcjaLokalna() throws InterruptedException {
      sprawdźPrzerwanie();
    }

    private static void sekcjaKrytyczna() throws InterruptedException {
      sprawdźPrzerwanie();
      ++licznik;
    }

    @Override
    public void run() {
      try {
        for (int i = 0; i < ITERACJE; ++i) {
          sekcjaLokalna();
          mutex.acquire();
          try {
            sekcjaKrytyczna();
          } finally {
            mutex.release();
          }
        }
      } catch (InterruptedException e) {
        Thread.currentThread().interrupt();
        System.err.println("Zwiększający przerwany");
      }
    }

  }

  public static void main(String args[]) {
    Thread pierwszy = new Thread(new Zwiększający());
    Thread drugi = new Thread(new Zwiększający());
    pierwszy.start();
    drugi.start();
    try {
      pierwszy.join();
      drugi.join();
      System.out.println(licznik); // 20000000
    } catch (InterruptedException e) {
      Thread.currentThread().interrupt();
      System.err.println("Główny przerwany");
    }
  }

}
