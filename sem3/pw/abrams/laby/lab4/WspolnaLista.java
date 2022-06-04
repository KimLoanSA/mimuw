import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class WspolnaLista {

  private static final int DODAWANE = 100000;

  private static final List<Integer> wspólna = Collections.synchronizedList(new ArrayList<>());

  private static void dodawaj() throws InterruptedException {
    for (int i = 0; i < DODAWANE; ++i) {
      if (Thread.interrupted()) {
        throw new InterruptedException();
      }
      wspólna.add(-1);
    }
  }

  public static void main(String[] args) {
    Thread główny = Thread.currentThread();
    Thread pomocniczy = new Thread(new Runnable() {

      @Override
      public void run() {
        try {
          dodawaj();
        } catch (InterruptedException e) {
          Thread.currentThread().interrupt();
          System.err.println("Pomocniczy przerwany");
          główny.interrupt();
        }
      }

    });
    pomocniczy.start();
    try {
      dodawaj();
      pomocniczy.join();
      int suma = 0;
      for (int x : wspólna) {
        suma += x;
      }
      System.out.println(wspólna.size() + " " + suma);
    } catch (InterruptedException e) {
      Thread.currentThread().interrupt();
      System.err.println("Główny przerwany");
    }
  }

}
