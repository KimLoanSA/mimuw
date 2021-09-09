
public class Skarb {
	private int rozmiar;
	
	public Skarb() {
		this.rozmiar = 0;
	}
	
	public Skarb(int rozmiar) {
		this.rozmiar = rozmiar;
	}
	
	public int rozmiar() {
		return rozmiar;
	}
	
	public void wypisz() {
		System.out.print(rozmiar);
	}
}
