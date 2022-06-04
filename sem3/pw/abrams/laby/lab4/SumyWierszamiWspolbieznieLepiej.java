
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.function.IntBinaryOperator;

public class SumyWierszamiWspolbieznieLepiej {

  private static final int WIERSZE = 10;
  private static final int KOLUMNY = 100;

  private static final ConcurrentMap<Integer, Integer> sumaWiersza = new ConcurrentHashMap<>();
  private static final ConcurrentMap<Integer, Integer> ileWKolumnie = new ConcurrentHashMap<>();

  private static void piszSumyWierszy(int wiersze, int kolumny, IntBinaryOperator macierz) {

    List<Thread> pomocnicze = new ArrayList<>();

    for (int k = 0; k < kolumny; k++) {
      pomocnicze.add(new Thread(new Liczacy(k, wiersze, macierz), "Liczacy:" + k));
    }

    for (Thread t : pomocnicze) {
      t.start();
    }

    try {
      for (Thread t : pomocnicze) {
        t.join();
      }

    } catch (InterruptedException e) {
      Thread.currentThread().interrupt();
      System.err.println("Główny przerwany");
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
      for (int w = 0; w < wiersze; w++ ) {
        sumaWiersza.putIfAbsent(w, 0);
        ileWKolumnie.putIfAbsent(w, 0);

        int valueToAddd = operator.applyAsInt(w, kolumna);

        sumaWiersza.computeIfPresent(w, (k, v) -> v + valueToAddd);
        ileWKolumnie.computeIfPresent(w, (k, v) -> v + 1);

        if (ileWKolumnie.getOrDefault(w, 0).equals(KOLUMNY)) {
          System.out.println(w + ": " + sumaWiersza.get(w));

          ileWKolumnie.remove(w);
          sumaWiersza.remove(w);
        }
      }
    }
  }

  public static void main(String[] args){
    piszSumyWierszy(WIERSZE, KOLUMNY, (wiersz, kolumna) -> {
      int a = 2 * kolumna + 1;
      try {
        Thread.sleep(1);
      } catch (Exception e){
        //empty
      }

      return (wiersz + 1) * (a % 4 - 2) * a;
    });
  }

}
