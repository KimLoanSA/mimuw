import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.RecursiveTask;

public class Fibonacci {

  private static final int LICZONE = 20;

  private static class Obliczenie extends RecursiveTask<Integer> {

    private static final long serialVersionUID = 1L;

    private final int argument;

    public Obliczenie(int argument) {
      this.argument = argument;
    }

    @Override
    protected Integer compute() {
      if (argument <= 1) {
        return argument;
      } else {
        Obliczenie prawy = new Obliczenie(argument - 2);
        prawy.fork();
        Obliczenie lewy = new Obliczenie(argument - 1);
        return lewy.compute() + prawy.join();
      }
    }

  }

  public static void main(String[] args) {
    ForkJoinPool pula = new ForkJoinPool();
    try {
      for (int i = 0; i < LICZONE; ++i) {
        RecursiveTask<Integer> obliczenie = new Obliczenie(i);
        int wynik = pula.invoke(obliczenie);
        System.out.println(i + " " + wynik);
      }
    } finally {
      pula.shutdown();
    }
  }

}
