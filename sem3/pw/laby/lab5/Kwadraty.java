import java.util.Random;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

public class Kwadraty {

  private static final int MAX_SEN = 10;
  private static final Random RANDOM = new Random();

  private static final int LICZONE = 10;
  private static final int WĄTKI = 8;

  private static void śpij() throws InterruptedException {
    int sen = RANDOM.nextInt(MAX_SEN);
    Thread.sleep(sen);
  }

  private static class Liczący implements Callable<Integer> {

    private final int wartość;

    private Liczący(int wartość) {
      this.wartość = wartość;
    }

    @Override
    public Integer call() throws InterruptedException {
      śpij();
      return wartość * wartość;
    }

  }

  private static class Piszący implements Runnable {

    private final Future<Integer> obietnica;

    public Piszący(Future<Integer> obietnica) {
      this.obietnica = obietnica;
    }

    @Override
    public void run() {
      try {
        śpij();
        System.out.println(obietnica.get());
      } catch (InterruptedException e) {
        Thread.currentThread().interrupt();
        System.err.println("Piszący przerwany");
      } catch (ExecutionException e) {
        Thread.currentThread().interrupt();
        System.err.println("Liczący przerwany");
      }
    }

  }

  public static void main(String[] args) {
    ExecutorService pula = Executors.newFixedThreadPool(WĄTKI);
    try {
      for (int i = 1; i <= LICZONE; ++i) {
        Callable<Integer> praca = new Liczący(i);
        Future<Integer> obietnica = pula.submit(praca);
        pula.submit(new Piszący(obietnica));
      }
    } finally {
      pula.shutdown();
    }
  }

}
