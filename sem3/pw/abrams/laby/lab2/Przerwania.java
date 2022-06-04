public class Przerwania {

  private static void czas() {
    System.out.println("aktualny czas: " + System.currentTimeMillis());
  }

  private static class Pomocniczy implements Runnable {

    private final int czasSnu;
    private final int czasObliczeń;

    public Pomocniczy(int czasSnu, int czasObliczeń) {
      this.czasSnu = czasSnu;
      this.czasObliczeń = czasObliczeń;
    }

    private static void komunikatZakończenia() {
      Thread aktualny = Thread.currentThread();
      System.out.println("wątek " + aktualny.getName() + " kończy pracę normalnie");
    }

    private static void komunikatPrzerwania(String s) {
      Thread aktualny = Thread.currentThread();
      System.out.print("wątek " + aktualny.getName());
      System.out.print(" przerwany podczas " + s);
      System.out.print(", flaga przed: " + aktualny.isInterrupted());
      Thread.currentThread().interrupt();
      System.out.println(", flaga po: " + aktualny.isInterrupted());
    }

    @Override
    public void run() {
      try {
        Thread.sleep(czasSnu);
        long przed = System.currentTimeMillis();
        while (System.currentTimeMillis() < przed + czasObliczeń) {
          if (Thread.interrupted()) {
            komunikatPrzerwania("obliczeń");
            return;
          }
          // obliczenia
        }
        komunikatZakończenia();
      } catch (InterruptedException e) {
        komunikatPrzerwania("snu");
      }
    }

  }

  public static void main(String args[]) {
    try {
      Thread pierwszy = new Thread(new Pomocniczy(1000, 1000), "Pierwszy");
      Thread drugi = new Thread(new Pomocniczy(3000, 1000), "Drugi");
      Thread trzeci = new Thread(new Pomocniczy(3000, 2000), "Trzeci");
      pierwszy.start();
      drugi.start();
      trzeci.start();
      czas();
      pierwszy.join();
      czas();
      drugi.interrupt();
      Thread.sleep(2000);
      czas();
      trzeci.interrupt();
    } catch (InterruptedException e) {
      Thread.currentThread().interrupt();
      System.out.println("Główny przerwany");
    }
  }

}