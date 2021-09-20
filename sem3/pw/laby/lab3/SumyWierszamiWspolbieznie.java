
import java.util.concurrent.BrokenBarrierException;
import java.util.concurrent.CyclicBarrier;
import java.util.function.IntBinaryOperator;

public class SumyWierszamiWspolbieznie {

  private static final int WIERSZE = 10;
  private static final int KOLUMNY = 1000;
  private static volatile int wiersz;
  private static int[] wynikKolumnny = new int[KOLUMNY];

  private static final CyclicBarrier doLiczenia = new CyclicBarrier(KOLUMNY,  new Sumujacy(KOLUMNY));

  private static void piszSumyWierszy(int wiersze, int kolumny, IntBinaryOperator macierz) {

    for (int k = 0; k < kolumny; k++) {
      new Thread(new Liczacy(k, wiersze, macierz), "Liczacy:" + k).start();
    }
  }


  private static class Liczacy implements Runnable {

    private final int kolumna;
    private final int wiersze;
    private final IntBinaryOperator operator;

    public Liczacy(int kolumna, int wiersze, IntBinaryOperator operator) {
      this.kolumna = kolumna;
      this.wiersze = wiersze;
      this.operator = operator;
    }

    @Override
    public void run() {
      while (wiersz < wiersze) {
        try {
          wynikKolumnny[kolumna] = operator.applyAsInt(wiersz, kolumna);
          doLiczenia.await();

        } catch (InterruptedException | BrokenBarrierException e) {
          Thread t = Thread.currentThread();
          t.interrupt();
          System.err.println(t.getName() + " przerwany");
        }
      }
    }
  }

  private static class Sumujacy implements Runnable {

    private final int kolumny;

    public Sumujacy(int kolumny) {
      this.kolumny = kolumny;
    }

    @Override
    public void run() {
      int suma = 0;
      for (int k = 0; k < kolumny; k++) {
        suma += wynikKolumnny[k];
      }

      System.out.println(wiersz + ": " + suma);
      wiersz++;
    }
  }


  public static void main(String[] args){
    piszSumyWierszy(WIERSZE, KOLUMNY, (wiersz, kolumna) -> {
      int a = 2 * kolumna + 1;
      try {
        Thread.sleep(400);
      } catch (Exception e){
        //empty
      }

      return (wiersz + 1) * (a % 4 - 2) * a;
    });
  }

}
