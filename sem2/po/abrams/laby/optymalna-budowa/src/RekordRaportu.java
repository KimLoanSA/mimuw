import java.util.ArrayList;

public class RekordRaportu {
	private Pret pret;
	private long odpad;
	private long cena;
	private ArrayList<Pret> czesci;
	
	//konstruktory
	public RekordRaportu() {
		this.czesci = new ArrayList<Pret> ();
	}
	
	public RekordRaportu(RekordCennika rekordCennika) {
		this.pret = rekordCennika.pret();
		this.odpad = rekordCennika.pret().dlugosc();
		this.czesci = new ArrayList<Pret> ();
		this.cena = rekordCennika.cena();
	}
	
	//gettery
	public long cena() {
		return cena;
	}
	
	public long odpad() {
		return odpad;
	}
	
	//porownywanie
	public boolean czyJestDluzszy(Pret argPret) {
		return argPret.porownaj(new Pret(odpad));
	}
	
	//dodaje do rekordu pret
	public void dodajCzesc(Pret dodawany) {
		czesci.add(dodawany);
		
		odpad -= dodawany.dlugosc();
	}
	
	//wypisywanie
	public void wypisz() {
		pret.wypisz();
		
		for (Pret akt : czesci) {
			akt.wypisz();
		}
		
		System.out.println(); 
	}
}

