import java.util.ArrayList;
import java.util.List;

public class TestPozwalacza {

  private static final int CYKLE = 3;
  private static final int WĄTKI = 10;
  private static final int KROK = 7;

  private static final Pozwalacz<Integer> pudełko = new Pozwalacz<>(0);

  private static class Pracownik implements Runnable {

    private final int numer;
    private final int następny;
    private final Thread główny;

    private Pracownik(int numer, int następny, Thread główny) {
      this.numer = numer;
      this.następny = następny;
      this.główny = główny;
    }

    @Override
    public void run() {
      try {
        for (int i = 0; i < CYKLE; ++i) {
          pudełko.pozwól(numer, następny);
        }
      } catch (InterruptedException e) {
        Thread.currentThread().interrupt();
        System.err.println("Pracownik" + numer + " przerwany");
        główny.interrupt();
      }
    }

  }

  public static void main(String[] args) {
    Thread główny = Thread.currentThread();
    List<Thread> pracownicy = new ArrayList<>();
    for (int i = 0; i < WĄTKI; ++i) {
      Thread t = new Thread(new Pracownik(i, (i + KROK) % WĄTKI, główny));
      pracownicy.add(t);
      t.start();
    }
    try {
      for (Thread t : pracownicy) {
        t.join();
      }
      for (int n : pudełko.dajHistorię()) {
        System.out.print(" " + n);
      }
      System.out.println();
    } catch (InterruptedException e) {
      główny.interrupt();
      for (Thread t : pracownicy) {
        t.interrupt();
      }
      System.err.println("Główny przerwany");
    }
  }

}
