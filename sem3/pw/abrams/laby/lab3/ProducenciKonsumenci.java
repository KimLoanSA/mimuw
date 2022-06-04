import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Semaphore;

public class ProducenciKonsumenci {

  private static final int DODATNI = 10;
  private static final int UJEMNI = 10;
  private static final int PRODUKOWANE = 5000;
  private static final int KONSUMENCI = 5;
  // (DODATNI + UJEMNI) * PRODUKOWANE % KONSUMENCI == 0
  private static final int KONSUMOWANE = ((DODATNI + UJEMNI) * PRODUKOWANE / KONSUMENCI);

  private static final int ROZMIAR = 10;

  private static int pierwszaZajęta = 0;
  private static int pierwszaWolna = 0;

  private static final int[] bufor = new int[ROZMIAR];

  private static final Semaphore zajęte = new Semaphore(0);
  private static final Semaphore wolne = new Semaphore(ROZMIAR);

  private static final Semaphore mutexZajętych = new Semaphore(1);
  private static final Semaphore mutexWolnych = new Semaphore(1);

  private static int get() throws InterruptedException {
    zajęte.acquire();
    mutexZajętych.acquire();
    int wynik = bufor[pierwszaZajęta];
    pierwszaZajęta = (pierwszaZajęta + 1) % ROZMIAR;
    mutexZajętych.release();
    wolne.release();
    return wynik;
  }

  private static void put(int x) throws InterruptedException {
    wolne.acquire();
    mutexWolnych.acquire();
    bufor[pierwszaWolna] = x;
    pierwszaWolna = (pierwszaWolna + 1) % ROZMIAR;
    mutexWolnych.release();
    zajęte.release();
  }

  private static class Producent implements Runnable {

    private final int mój;

    public Producent(int mój) {
      this.mój = mój;
    }

    @Override
    public void run() {
      try {
        for (int i = 0; i < PRODUKOWANE; ++i) {
          put(mój);
        }
      } catch (InterruptedException e) {
        Thread t = Thread.currentThread();
        t.interrupt();
        System.err.println(t.getName() + " przerwany");
      }
    }

  }

  private static class Konsument implements Runnable {

    @Override
    public void run() {
      Thread t = Thread.currentThread();
      try {
        int suma = 0;
        int pobrane = 0;
        for (int i = 0; i < KONSUMOWANE; ++i) {
          int x = get();
          suma += x;
          ++pobrane;
        }
        System.out.println(t.getName() + " pobrał: " + pobrane + ", suma: " + suma);
      } catch (InterruptedException e) {
        t.interrupt();
        System.err.println(t.getName() + " przerwany");
      }
    }

  }

  public static void main(String args[]) {
    List<Thread> wątki = new ArrayList<>();
    for (int i = 0; i < DODATNI; ++i) {
      Runnable r = new Producent(1);
      Thread t = new Thread(r, "Dodatni" + i);
      wątki.add(t);
    }
    for (int i = 0; i < UJEMNI; ++i) {
      Runnable r = new Producent(-1);
      Thread t = new Thread(r, "Ujemny" + i);
      wątki.add(t);
    }
    for (int i = 0; i < KONSUMENCI; ++i) {
      Runnable r = new Konsument();
      Thread t = new Thread(r, "Konsument" + i);
      wątki.add(t);
    }
    for (Thread t : wątki) {
      t.start();
    }
    try {
      for (Thread t : wątki) {
        t.join();
      }
    } catch (InterruptedException e) {
      Thread.currentThread().interrupt();
      System.err.println("Główny przerwany");
    }
  }

}
