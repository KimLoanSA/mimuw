import java.util.ArrayList;

public class Raport {
	private ArrayList<RekordRaportu> rekordy;
	
	//konstruktor
	public Raport() {
		rekordy = new ArrayList<RekordRaportu> ();
	}
	
	//tworzenie nowego rekordu
	public void nowyRekord(RekordCennika argRekordCennika) {
		rekordy.add(new RekordRaportu(argRekordCennika));
	}
	
	//sprawdzanie czy w ostatnim rekordzie zmiesci sie nowy pret
	public boolean czyDaSieDodacDoOstatniegoRekordu(Pret argPret) {
		if (rekordy.size() > 0) {
			return rekordy.get(rekordy.size() - 1).czyJestDluzszy(argPret);
		}
		
		return false;
	}
	
	//dodawanie do ostatniego rekordu pretu
	public void dodajDoOstatniegoRekordu(Pret argPret) {
		if (rekordy.size() > 0) {
			rekordy.get(rekordy.size() - 1).dodajCzesc(argPret);
		}
	}
	
	//dodawanie calego rekordu
	public void dodajCalyRekord(RekordRaportu argRekord) {
		rekordy.add(argRekord);
	}
	
	//obliczanie ceny i odpadow dla calego raportu
	private void koszta() {
		long cena = 0;
		long odpad = 0;
		
		for (RekordRaportu aktRek : rekordy) {
			cena += aktRek.cena();
			odpad += aktRek.odpad();
		}
		
		System.out.println(cena);
		System.out.println(odpad);
	}

	//wypisywanie
	public void wypisz() {
		koszta();

		for (RekordRaportu aktRek : rekordy) {
			aktRek.wypisz();
		}
	}
}
