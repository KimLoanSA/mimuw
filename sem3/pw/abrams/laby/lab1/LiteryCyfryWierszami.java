public class LiteryCyfryWierszami {

  private static volatile boolean literySkończył = false;
  private static volatile boolean cyfrySkończył = false;

  private static volatile boolean aktualny;

  private static class Piszący implements Runnable {

    private static final int N_WIERSZY = 100;
    private static final int W_WIERSZU = 50;

    private final char pierwszy;
    private final char ostatni;
    private final boolean ja;

    public Piszący(char pierwszy, char ostatni, boolean ja) {
      this.pierwszy = pierwszy;
      this.ostatni = ostatni;
      this.ja = ja;
    }

    @Override
    public void run() {
      char c = pierwszy;
      for (int i = 0; i < N_WIERSZY; ++i) {
        while (aktualny != ja) {
          // empty
        }
        for (int j = 0; j < W_WIERSZU; ++j) {
          System.out.print(c);
          ++c;
          if (c > ostatni) {
            c = pierwszy;
          }
        }
        System.out.println();
        aktualny = !aktualny;
      }
      if (ja) {
        literySkończył = true;
      } else {
        cyfrySkończył = true;
      }
    }

  }

  public static void main(String args[]) {
    Thread litery = new Thread(new Piszący('a', 'z', true));
    Thread cyfry = new Thread(new Piszący('0', '9', false));
    aktualny = true;
    System.out.println("Początek");
    litery.start();
    cyfry.start();
    while (!literySkończył) {
      // pusta
    }
    while (!cyfrySkończył) {
      // pusta
    }
    System.out.println("Koniec");
  }

}