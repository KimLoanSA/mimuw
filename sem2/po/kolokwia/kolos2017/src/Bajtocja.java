
public class Bajtocja {
	public int[] przeprowadzTydzien(Firma[] firmy) {
		int[] wynik = new int[firmy.length];
		
		for (int dzien = 0; dzien < 7; dzien++) {
			for (Firma aktF : firmy) {
				aktF.dzien(dzien);
			}
		}
		
		for (int i = 0; i < firmy.length; i++) {
			wynik[i] = firmy[i].pestki();
		}
		
		return wynik;
	}
}
