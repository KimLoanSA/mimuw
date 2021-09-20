import java.util.List;

public class Cw2v2 {

  private final static List<Integer> PRIMARY = List.of(2, 3, 5, 7, 11, 13, 17, 19, 23, 29);
  private final static List<Integer> STARTING_NUMBERS = List.of(31, 37, 41, 43, 47, 49, 53, 59);
  private static final int DIFF = 30;

  private static volatile boolean found;
  private static volatile int count;

  private static class CheckingPrime implements Runnable {

    private final int starting;
    private final int n;

    public CheckingPrime(int n, int starting) {
      this.starting = starting;
      this.n = n;
    }

    @Override
    public void run() {
      checkChain(n);
      count++;
    }

    private void checkChain(int n) {
      int element = starting;

      while (!found && isPrimeSmaller(n, element)) {
        if (checkRest(n, element)) {
          element = nextElem(element);
        } else {
          found = true;
        }
      }
    }

    private int nextElem(int a) {
      return a + DIFF;
    }

    private boolean checkRest(int n, int p) {
      return n % p != 0;
    }

    private boolean isPrimeSmaller(int n, int p) {
      return n > p;
    }
  }


  public static void main(String[] args) {
    System.out.println(test());
  }

  private static int test() {
    int counter = 0;

    for (int i = 2; i <= 10000; i++) {
      counter += check(i) ? 1 : 0;
    }

    return counter;
  }


  public static boolean check(int n) {
    if (!checkSmall(n)) {
      return false;
    }

    count = 0;
    found = false;
    STARTING_NUMBERS.forEach(p -> startThread(n, p));

    while (count != STARTING_NUMBERS.size()) {
      Thread.yield();
    }

    return !found;
  }

  private static boolean checkSmall(Integer n) {
    for (Integer p : PRIMARY) {
      if (n % p == 0 && !n.equals(p)) {
        return false;
      }
    }
    return true;
  }

  private static void startThread(int n, int starting) {
    Runnable runnable = new CheckingPrime(n, starting);
    Thread thread = new Thread(runnable, "Checking for starting number: " + n + "with prime: " + starting);
    thread.start();

    try {
      thread.join();
    } catch (Exception e) {
      System.out.println("WTF");
    }
  }
}
