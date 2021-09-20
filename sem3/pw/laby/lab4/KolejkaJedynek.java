import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

public class KolejkaJedynek {

  private static final int PRODUKOWANE = 100000;

  private static final BlockingQueue<Integer> kolejka = new LinkedBlockingQueue<>();

  public static void main(final String[] args) {
    Thread główny = Thread.currentThread();
    Thread pomocniczy = new Thread(new Runnable() {

      @Override
      public void run() {
        try {
          for (int i = 0; i < PRODUKOWANE; ++i) {
            kolejka.put(1);
          }
        } catch (InterruptedException e) {
          Thread.currentThread().interrupt();
          System.err.println("Pomocniczy przerwany");
          główny.interrupt();
        }
      }

    });
    pomocniczy.start();
    try {
      int suma = 0;
      for (int i = 0; i < PRODUKOWANE; ++i) {
        suma += kolejka.take();
      }
      System.out.println(suma);
      pomocniczy.join();
    } catch (InterruptedException e) {
      Thread.currentThread().interrupt();
      System.err.println("Główny przerwany");
    }
  }

}
