
public class Przedmiot {
	private int rok;
	private String kraj;
	private int cena;
	
	
	public Przedmiot(int rok, String kraj, int cena) {
		this.rok = rok;
		this.kraj = kraj;
		this.cena = cena;
	}
	
	public int cena() {
		return cena;
	}
	
	public String kraj() {
		return kraj;
	}
	
	public int rok() {
		return rok;
	}
}
