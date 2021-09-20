import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

public class Piramida {

  private static final int SUMOWANE = 100;
  private static final int WĄTKI = 8;

  private static class Kwadrat implements Callable<Integer> {

    private final int wartość;

    private Kwadrat(int wartość) {
      this.wartość = wartość;
    }

    @Override
    public Integer call() {
      return wartość * wartość;
    }

  }

  public static void main(String[] args) {
    ExecutorService pula = Executors.newFixedThreadPool(WĄTKI);
    List<Callable<Integer>> obliczenia = new ArrayList<>();
    for (int i = 1; i <= SUMOWANE; ++i) {
      Callable<Integer> praca = new Kwadrat(i);
      obliczenia.add(praca);
    }
    try {
      List<Future<Integer>> obietnice = pula.invokeAll(obliczenia);
      int suma = 0;
      for (Future<Integer> kolejna : obietnice) {
        suma += kolejna.get();
      }
      System.out.println(suma + " " + SUMOWANE * (SUMOWANE + 1) * (2 * SUMOWANE + 1) / 6);
    } catch (InterruptedException | ExecutionException e) {
      Thread.currentThread().interrupt();
      System.err.println("Obliczenie przerwane");
    } finally {
      pula.shutdown();
    }
  }

}
