
public class RekordCennika {
	private Pret pret;
	private long cena; //long, bo w wyniku moze przekroczyc inta
	
	//konstruktory
	public RekordCennika() {
		this.pret = new Pret();
		this.cena = 0;
	}
	
	public RekordCennika(Pret pret, long cena) {
		this.pret = pret;
		this.cena = cena;
	}
	
	//gettery
	public Pret pret() {
		return pret;
	}
	
	public long cena() {
		return cena;
	}
}
