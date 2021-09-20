import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.atomic.AtomicInteger;

public class Liczniki {

  private static final int POMOCNICZE = 10;
  private static final int LICZNIKI = POMOCNICZE * POMOCNICZE;
  private static final int LIMIT = LICZNIKI * LICZNIKI;

  private static final ConcurrentMap<Integer, AtomicInteger> liczniki = new ConcurrentHashMap<>();

  private static class Pomocniczy implements Runnable {

    private final int start;

    public Pomocniczy(int start) {
      this.start = start;
    }

    @Override
    public void run() {
      for (int i = start; i < LIMIT; i += POMOCNICZE) {
        int x = i % LICZNIKI;
        AtomicInteger a = liczniki.computeIfAbsent(x, (k) -> new AtomicInteger());
        a.incrementAndGet();
      }
    }

  }

  public static void main(String[] args) {
    List<Thread> pomocnicze = new ArrayList<>();
    for (int i = 0; i < POMOCNICZE; ++i) {
      pomocnicze.add(new Thread(new Pomocniczy(i)));
    }
//    for (Thread t : pomocnicze) {
//      t.start();
//    }
//    try {
//      for (Thread t : pomocnicze) {
//        t.join();
//      }
//      Set<Integer> klucze = new TreeSet<>();
//      Set<Integer> wartości = new TreeSet<>();
//      for (Map.Entry<Integer, AtomicInteger> entry : liczniki.entrySet()) {
//        klucze.add(entry.getKey());
//        wartości.add(entry.getValue().get());
//      }
//      System.out.println(klucze);
//      System.out.println(wartości);
//    } catch (InterruptedException e) {
//      Thread.currentThread().interrupt();
//      System.err.println("Główny przerwany");
//    }
  }

}
