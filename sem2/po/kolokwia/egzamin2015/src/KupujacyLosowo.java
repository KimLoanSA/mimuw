import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Random;

public class KupujacyLosowo extends Kupujacy {
    public KupujacyLosowo(ArrayList<Wymaganie> wymagania) {
        super(wymagania);
    }

    @Override
    public ArrayList<Ogloszenie> wybierz() {
        ArrayList<Ogloszenie> wybrane = System.wybierz(super.wymagania);
        Collections.shuffle(wybrane);

        return new ArrayList<>(wybrane.subList(0, 5));
    }
}


