import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.RecursiveAction;

public class Jedynkowanie {

  private static final int ROZMIAR = 1000000;
  private static final int LIMIT = 10;

  private static class Akcja extends RecursiveAction {

    private static final long serialVersionUID = 1L;

    private final int[] tablica;
    private final int początek;
    private final int zaKońcem;
    private final int limit;

    public Akcja(int[] tablica, int początek, int zaKońcem, int limit) {
      this.tablica = tablica;
      this.początek = początek;
      this.zaKońcem = zaKońcem;
      this.limit = limit;
    }

    @Override
    protected void compute() {
      if (zaKońcem - początek < limit) {
        for (int i = początek; i < zaKońcem; ++i) {
          tablica[i] = 1;
        }
      } else {
        int środek = (zaKońcem + początek) / 2;
        Akcja prawa = new Akcja(tablica, środek, zaKońcem, limit);
        prawa.fork();
        Akcja lewa = new Akcja(tablica, początek, środek, limit);
        lewa.compute();
        prawa.join();
      }
    }

  }

  public static void main(String[] args) {
    int[] tablica = new int[ROZMIAR];
    ForkJoinPool pula = new ForkJoinPool();
    try {
      Akcja jedynkowanie = new Akcja(tablica, 0, ROZMIAR, LIMIT);
      pula.invoke(jedynkowanie);
      int suma = 0;
      for (int x : tablica) {
        suma += x;
      }
      System.out.println(suma);
    } finally {
      pula.shutdown();
    }
  }

}
