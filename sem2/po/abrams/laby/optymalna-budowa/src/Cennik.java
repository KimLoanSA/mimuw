import java.util.Scanner;

public class Cennik {
	private RekordCennika[] rekordy;
	private int rozmiar;
	
	//wczytywanie
	public void wczytaj(Scanner wejscie) {
		int dlugosc;
		int cena;
		
		rozmiar = wejscie.nextInt();
		rekordy = new RekordCennika[rozmiar];
		
		for (int i = 0; i < rozmiar; i++) {
			dlugosc = wejscie.nextInt();
			cena = wejscie.nextInt();
			
			rekordy[i] = new RekordCennika(new Pret(dlugosc), cena);
		}
	}
	
	//getter
	public int rozmiar() {
		return rozmiar;
	}
		
	//znajduje rekord, w ktorym jest najtanszy pret dluzszy lub rowny od danego preta
	public RekordCennika znajdzZNajmniejszaCena(Pret argPret) {
		long cena = -1;
		RekordCennika wynikRekord = new RekordCennika(new Pret(), -1);
		
		for (int i = 0; i < rozmiar; i++) {
			if (argPret.porownaj(rekordy[i].pret())) {
				if (cena == -1) {
					cena = rekordy[i].cena();
				}
				
				if (rekordy[i].cena() <= cena) {
					cena = rekordy[i].cena();
					wynikRekord = rekordy[i];
				}
			}
		}
		
		return wynikRekord;
	}
	
	//znajduje rekord, w ktorym jest pret pokrywajacy dany i dajacy najmniejszy odpad
	public RekordCennika znajdzZNajmniejszymOdpadem(Pret argPret) {
		long odpad = -1;
		RekordCennika wynikRekord = new RekordCennika(new Pret(-1), -1);
		
		for (int i = 0; i < rozmiar; i++) {
			if (argPret.porownaj(rekordy[i].pret())) {
				if (odpad == -1) {
					odpad = rekordy[i].pret().dlugosc() - argPret.dlugosc();
				}
				
				if (rekordy[i].pret().dlugosc() - argPret.dlugosc() <= odpad) {
					odpad = rekordy[i].pret().dlugosc() - argPret.dlugosc();
					wynikRekord = rekordy[i];
				}
			}
		}
		
		return wynikRekord;
	}
	
	//algorytm wyszukiwania binarnego
	private int lowerBound(Pret szukany) {
		int lewy = 0;
		int prawy = rozmiar;
		
		while (lewy < prawy) {
			int srodek = (lewy + prawy) / 2;
			
			if (szukany.porownaj(rekordy[srodek].pret())) {
				prawy = srodek;
			}
			else {
				lewy = srodek + 1;
			}
		}
		
		return lewy;
	}
	
	//znajduje rekord w ktorym jest pret wiekszy rowny od szukanego
	public RekordCennika dajWiekszyRowny(Pret szukany) {
		int indeks = lowerBound(szukany);
		
		if (indeks < 0 || indeks >= rozmiar) {
			return null;
		}
		
		return rekordy[indeks];
	}
	
	//daje najwiekszy z rekordow
	public RekordCennika dajNajwiekszy() {
		return rekordy[rozmiar -1];
	}
}
