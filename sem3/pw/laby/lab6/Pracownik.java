public class Pracownik implements Runnable {

  private final int grupa;
  private final Serwer serwer;

  private void własneSprawy() throws Exception{
//    Thread.sleep(1);
  }

  private void korzystam(int zasób) {
    System.out.println(grupa + ": " + zasób);
  }

  public Pracownik(int grupa, Serwer serwer) {
    this.grupa = grupa;
    this.serwer = serwer;
  }

  @Override
  public void run() {
    try {
      while (true) {
        własneSprawy();
        int zasób = serwer.chcęKorzystać(grupa);
        korzystam(zasób);
        serwer.skończyłem(grupa, zasób);
      }
    } catch (Exception e) {
      Thread.currentThread().interrupt();
      System.err.println("Pracownik grupy " + grupa + " przerwany");
    }
  }
}
