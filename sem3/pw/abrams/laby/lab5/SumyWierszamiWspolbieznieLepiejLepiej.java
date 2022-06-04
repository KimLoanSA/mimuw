
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.function.IntBinaryOperator;

public class SumyWierszamiWspolbieznieLepiejLepiej {

  private static final int WIERSZE = 10;
  private static final int KOLUMNY = 11;
  private static final int WATKI  = 4;

  private static final ConcurrentMap<Integer, Integer> macierz = new ConcurrentHashMap<>();

  private static class ElMac {
    int wiersz;
    int kolumna;
    int wartosc;

    public ElMac(int wiersz, int kolumna, int wartosc) {
      this.wiersz = wiersz;
      this.kolumna = kolumna;
      this.wartosc = wartosc;
    }

    public int getWiersz() {
      return wiersz;
    }

    public int getWartosc() {
      return wartosc;
    }

    public int getKolumna() {
      return kolumna;
    }
  }

  private static class Liczący implements Callable<ElMac> {

    private final int wiersz;
    private final int kolumna;
    private final IntBinaryOperator operator;

    public Liczący(int wiersz, int kolumna, IntBinaryOperator operator) {
      this.wiersz = wiersz;
      this.kolumna = kolumna;
      this.operator = operator;
    }

    @Override
    public ElMac call() throws Exception {
      return new ElMac(wiersz, kolumna, operator.applyAsInt(wiersz, kolumna));
    }
  }

  private static void piszSumyWierszy(int wiersze, int kolumny, IntBinaryOperator op) {
    ExecutorService pula = Executors.newFixedThreadPool(WATKI);
    List<Callable<ElMac>> obliczenia = new ArrayList<>();

    for (int w = 0; w < WIERSZE; w++) {
      for (int k = 0; k < KOLUMNY; k++) {
        obliczenia.add(new Liczący(w, k, op));
      }
    }

    try {
        List<Future<ElMac>> wyniki = pula.invokeAll(obliczenia);
        int suma = 0;

        for (Future<ElMac> akt : wyniki) {
          ElMac obliczone = akt.get();

          suma += obliczone.getWartosc();

          if (obliczone.getKolumna() == KOLUMNY - 1) {
            System.out.println(obliczone.getWiersz() + ": " + suma);

            suma = 0;
          }

        }

    } catch (Exception e) {
      // empty
    } finally {
      pula.shutdown();
    }
  }


  public static void main(String[] args) {
    piszSumyWierszy(WIERSZE, KOLUMNY, (wiersz, kolumna) -> {
      int a = 2 * kolumna + 1;
//      try {
//        Thread.sleep(1);
//      } catch (Exception e){
//        //empty
//      }

      return (wiersz + 1) * (a % 4 - 2) * a;
    });
  }

}
