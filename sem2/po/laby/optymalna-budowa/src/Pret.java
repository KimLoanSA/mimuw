
public class Pret {
	private long dlugosc; //long, bo wynik moze przzekraczac inta
	
	//konstruktory
	public Pret() {
		this.dlugosc = 0;
	}
	
	public Pret(long dlugosc) {
		this.dlugosc = dlugosc;
	}
	
	//gettery
	public long dlugosc() {
		return dlugosc;
	}
	
	//operacje
	public boolean porownaj(Pret argPret) {
		if (dlugosc <= argPret.dlugosc()) 
			return true;
		
		return false;
	}
	
	public void wypisz() {
		System.out.print(dlugosc + " ");
	}
}
