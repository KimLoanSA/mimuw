import java.util.ArrayList;

public class System {
    private static ArrayList<Ogloszenie> ogloszenia;

    public static ArrayList<Ogloszenie> wybierz(ArrayList<Wymaganie> wymagania) {
        ArrayList<Ogloszenie> wynik = new ArrayList<>();

        for (Wymaganie aktWymag : wymagania) {
            for (Ogloszenie aktOgloszenie : ogloszenia) {
                if (aktWymag.czyPasuje(aktOgloszenie)) {
                    wynik.add(aktOgloszenie);
                }
            }
        }

        return wynik;
    }

    public static void przyjmijOgloszenie(Ogloszenie stareOgloszenie) {
        //
    }
}
