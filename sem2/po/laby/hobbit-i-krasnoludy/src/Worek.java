import java.util.Arrays;

public class Worek {
	private int pojemnosc;
	private int aktualnyRozmiar;
	private Skarb[] listaSkarbow = new Skarb[1];
	private int indeks = 0;	
	
	public Worek(int pojemnosc) {
		this.pojemnosc = pojemnosc;
	}
	
	public boolean dodaj(Skarb s) { //true - udalo sie
		aktualnyRozmiar += s.rozmiar();
		listaSkarbow[indeks] = s;
		indeks++;
		
		if (indeks == listaSkarbow.length) {
			listaSkarbow = Arrays.copyOf(listaSkarbow, listaSkarbow.length * 2);
			
		}
		
		if (aktualnyRozmiar <= pojemnosc) {
			return true;
		}
		
		return false;
	}
	
	public void wypisz() {
		for (int i = 0; i < indeks; i++) {
			System.out.print(listaSkarbow[i] + "(");
			listaSkarbow[i].wypisz();
			System.out.print("), ");
		}
	}
	
	public int pojemnosc() {
		return pojemnosc;
	}
	
}
