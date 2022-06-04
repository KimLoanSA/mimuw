import java.util.Scanner;

public class Rozwiazanie {
	public static void rozwiaz() {
		Cennik cennik = new Cennik();
		Projekt projekt = new Projekt();
		Scanner wejscie = new Scanner(System.in);
		Strategia strategia;
		
		cennik.wczytaj(wejscie);
		projekt.wczytaj(wejscie);

		wejscie.nextLine();
		String rodzaj = wejscie.nextLine();
		
		if (rodzaj.equals("minimalistyczna")) {
			strategia = new StrategiaMinimalistyczna();
		}
		else if (rodzaj.equals("maksymalistyczna")) {
			strategia = new StrategiaMaksymalistyczna();
		}
		else if (rodzaj.equals("ekonomiczna")) {
			strategia = new StrategiaEkonomiczna();
		}
		else if (rodzaj.equals("ekologiczna")){
			strategia = new StrategiaEkologiczna();
		}
		else {
			return;	
		}
		
		strategia.rozwiaz(cennik, projekt);
		strategia.wypisz();
	}
}
