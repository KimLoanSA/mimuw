import java.util.Arrays;

public class Serwer {

  private final int zasoby;
  private final int grupy;

  int ileUzytych = 0;

  private int ktoryUzywaja[];
  private boolean czyZasobZajety[];
  private int ileWGrupie[];

  public Serwer(int zasoby, int grupy) {
    this.zasoby = zasoby;
    this.grupy = grupy;

    this.ktoryUzywaja = new int[grupy];
    this.ileWGrupie = new int[grupy];
    this.czyZasobZajety = new boolean[zasoby];

    Arrays.fill(ktoryUzywaja, -1);
  }

  public synchronized int chcęKorzystać(int grupa) throws InterruptedException {
    ileWGrupie[grupa]++;

    if (ktoryUzywaja[grupa] != -1) {
      return ktoryUzywaja[grupa];
    } else if (ileWGrupie[grupa] == 1) {
      while(ileUzytych == zasoby) {
        wait();
      }

      ileUzytych++;

      for (int i = 0; i < zasoby && ktoryUzywaja[grupa] == -1; i++) {
        if (!czyZasobZajety[i]) {
          czyZasobZajety[i] = true;
          ktoryUzywaja[grupa] = i;
        }
      }

      notifyAll();

    } else {
      while (ktoryUzywaja[grupa] == -1) {
        wait();
      }
    }

    System.out.println("DAJE: " + ktoryUzywaja[grupa]);

    return ktoryUzywaja[grupa];
  }

  public synchronized void skończyłem(int grupa, int zasob) {
    System.out.println("ZWALNIAM:" + grupa);
    ileWGrupie[grupa]--;

    if (ileWGrupie[grupa] == 0) {
      czyZasobZajety[zasob] = false;
      ktoryUzywaja[grupa] = -1;
      ileUzytych--;

      notifyAll();
    }
  }

}
