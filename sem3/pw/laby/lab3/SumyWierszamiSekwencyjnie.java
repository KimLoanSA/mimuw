import java.util.function.IntBinaryOperator;

public class SumyWierszamiSekwencyjnie {

  private static final int WIERSZE = 10;
  private static final int KOLUMNY = 100;

  private static void piszSumyWierszy(int wiersze, int kolumny, IntBinaryOperator macierz) {
    for (int w = 0; w < wiersze; ++w) {
      int suma = 0;
      for (int k = 0; k < kolumny; ++k) {
        suma += macierz.applyAsInt(w, k);
      }
      System.out.println(w + " " + suma);
    }
  }

  public static void main(String[] args) {
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
