public class SprzedajacyNiecierpliwy extends OsobaFizyczna {

    private int tygodnie;

    @Override
    public void aktualizuj(Ogloszenie ogloszenie) {
        if (tygodnie % 8 == 0) {
            ogloszenie.obnizCeneO(5);
            System.przyjmijOgloszenie(ogloszenie);
        }
    }
}
