import java.util.ArrayList;

public class Ogloszenie {

    private Mieszkanie mieszkanie;
    private int cena;
    private Data dataDodania;
    private long nrTelefonu;
    private ArrayList<Zdjecie> zdjecia;

    public boolean czyJestZdjecie() {
        return zdjecia.isEmpty();
    }

    public ArrayList<Zdjecie> getZdjecia() {
        return zdjecia;
    }

    public int pokoje() {
        return mieszkanie.getLiczbaPokoi();
    }

    public void obnizCeneO(int procent) {
        cena -= cena * procent / 100;
    }
}
