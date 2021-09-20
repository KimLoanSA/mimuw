import java.util.ArrayList;
import java.util.List;

public class TestSerwer {

  private static final int ZASOBY = 2;
  private static final int GRUPY = 10;

  public static void main(String[] args) {
    Thread główny = Thread.currentThread();
    List<Thread> pracownicy = new ArrayList<>();
    Serwer serwer = new Serwer(ZASOBY, GRUPY);

    for (int i = 0; i < GRUPY; ++i) {
      for (int j = 0; j <= i; j++) {
        Thread t = new Thread(new Pracownik(i, serwer));
        t.start();
      }
    }
    try {
      for (Thread t : pracownicy) {
        t.join();
      }
    } catch (InterruptedException e) {
      główny.interrupt();
      for (Thread t : pracownicy) {
        t.interrupt();
      }
      System.err.println("Główny przerwany");
    }
  }

}

