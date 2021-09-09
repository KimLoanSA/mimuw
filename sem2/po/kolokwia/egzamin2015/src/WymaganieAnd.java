public class WymaganieAnd extends Wymaganie {

    private Wymaganie wymaganie1;
    private Wymaganie wymaganie2;

    public WymaganieAnd(Wymaganie wymaganie1, Wymaganie wymaganie2) {
        this.wymaganie1 = wymaganie1;
        this.wymaganie2 = wymaganie2;
    }

    @Override
    public boolean czyPasuje(Ogloszenie ogloszenie) {
        return wymaganie1.czyPasuje(ogloszenie) && wymaganie2.czyPasuje(ogloszenie);
    }
}
