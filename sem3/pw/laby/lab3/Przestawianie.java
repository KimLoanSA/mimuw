import java.util.Random;
import java.util.concurrent.BrokenBarrierException;
import java.util.concurrent.CyclicBarrier;

public class Przestawianie {

  private static final int OPÓŹNIENIE = 2;
  private static final int PRZESTAWIANE = 10;

  private static final Random RANDOM = new Random();

  private static final int[] dane = new int[PRZESTAWIANE];

  private static final CyclicBarrier pierwsza;
  private static final CyclicBarrier druga;

  static {
    pierwsza = new CyclicBarrier(PRZESTAWIANE);
    druga = new CyclicBarrier(PRZESTAWIANE, new Runnable() {

      @Override
      public void run() {
        for (int x : dane) {
          System.out.print(" " + x);
        }
        System.out.println();
      }

    });
  }

  private static void śpij() throws InterruptedException {
    Thread.sleep(RANDOM.nextInt(OPÓŹNIENIE));
  }

  private static class Pomocniczy implements Runnable {

    private final int moja;

    public Pomocniczy(int moja) {
      this.moja = moja;
    }

    @Override
    public void run() {
      try {
        śpij();
        dane[moja] = moja + 1;
        pierwsza.await();
        śpij();
        int x = dane[PRZESTAWIANE - 1 - moja];
        pierwsza.await();
        śpij();
        dane[moja] = x;
        druga.await();
      } catch (InterruptedException | BrokenBarrierException e) {
        Thread t = Thread.currentThread();
        t.interrupt();
        System.err.println(t.getName() + " przerwany");
      }
    }

  }

  public static void main(final String[] args) {
    for (int i = 0; i < PRZESTAWIANE; ++i) {
      new Thread(new Pomocniczy(i), "Pomocniczy" + i).start();
    }
  }

}
