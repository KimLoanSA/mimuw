import java.util.ArrayList;

public class KupujacyEsteta extends Kupujacy {

    public KupujacyEsteta(ArrayList<Wymaganie> wymagania) {
        super(wymagania);
    }

    private boolean czyDobreZdjecia(ArrayList<Zdjecie> zdjecia) {
        return true;
    }

    @Override
    public ArrayList<Ogloszenie> wybierz() {
        ArrayList<Ogloszenie> wynik = new ArrayList<>();
        ArrayList<Ogloszenie> spelniajaceWymag = System.wybierz(super.wymagania);

        for (Ogloszenie aktOgl : spelniajaceWymag) {
            if (aktOgl.czyJestZdjecie()) {
                if (czyDobreZdjecia(aktOgl.getZdjecia())) {
                    wynik.add(aktOgl);
                }
            }
        }

        return wynik;
    }
}
