public class WymaganiePokoje extends Wymaganie {

    private int wymaganePokoje;

    public WymaganiePokoje(int wymaganePokoje) {
        this.wymaganePokoje = wymaganePokoje;
    }

    @Override
    public boolean czyPasuje(Ogloszenie ogloszenie) {
        return ogloszenie.pokoje() == wymaganePokoje;
    }
}
