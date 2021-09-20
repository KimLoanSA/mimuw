public class Wiele {

  private static void pisz() {
    System.out.println(Thread.currentThread());
  }

  private static class Potokowy implements Runnable {

    private int ile;

    public Potokowy(int ile) {
      this.ile = ile;
    }

    @Override
    public void run() {
      if (ile > 0) {
        Runnable r = new Potokowy(ile - 1);
        Thread t = new Thread(r, "Pomocniczy: " + ile);
        t.start();
      }

      if (ile == 2) {
        try {
          Thread.sleep(10000);
        } catch (Exception e) {
        }
      }

      pisz();
    }
  }

  public static void main(String[] args) {
    Thread.currentThread().setName("Główny");
    Runnable r = new Potokowy(5);
    Thread t = new Thread(r, "Pomocniczy start");
    t.start();
    pisz();
  }

}