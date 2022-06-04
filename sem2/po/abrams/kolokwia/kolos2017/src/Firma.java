
public class Firma {
	private Statek[] statki;
	
	public void dzien(int numerDnia) {
		for (Statek akt : statki) {
			akt.dzien(numerDnia);
		}
	}
	
	public int pestki() {
		int wynik = 0;

		for (Statek akt : statki) {
			wynik += akt.liczbaPestek();
		}
		
		return wynik;
	}
}
