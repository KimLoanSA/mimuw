import java.util.ArrayList;

public class KupujacyWszystkie extends Kupujacy {

    public KupujacyWszystkie(ArrayList<Wymaganie> wymagania) {
        super(wymagania);
    }

    @Override
    public ArrayList<Ogloszenie> wybierz() {
        return System.wybierz(super.wymagania);
    }
}
