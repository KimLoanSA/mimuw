import java.util.Scanner;

public class Projekt {
	private Pret[] prety;
	private int rozmiar;
	
	//gettery
	public int rozmiar() {
		return rozmiar;
	}
	
	public Pret pretNr(int numer) {
		if (numer >= rozmiar || numer < 0) {
			return null;
		}
		
		return prety[numer];
	}	
	
	//wczytywanie
	public void wczytaj(Scanner wejscie) {
		int dlugosc;
		
		rozmiar = wejscie.nextInt();
		prety = new Pret[rozmiar];
		
		for (int i = 0; i < rozmiar; i++) {
			dlugosc = wejscie.nextInt();
			
			prety[i] = new Pret(dlugosc);
		}
	}
}
