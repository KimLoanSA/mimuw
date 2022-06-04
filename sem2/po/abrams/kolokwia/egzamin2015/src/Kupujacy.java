import java.util.ArrayList;

public abstract class Kupujacy {
    protected ArrayList<Wymaganie> wymagania;

    public Kupujacy(ArrayList<Wymaganie> wymagania) {
        this.wymagania = wymagania;
    }

    public abstract ArrayList<Ogloszenie> wybierz();
}
