import java.util.Arrays;

public class FunTab extends Funkcja {
	private int[] dziedzina;
	private int[] wartosci;
	private int ostatni;
	
	public FunTab(int[] dziedzina, int[] wartosci) {
		this.dziedzina = dziedzina;
		this.wartosci = wartosci;
	}
	
	public FunTab(Funkcja f) {
		this.dziedzina = f.dziedzina();
		int[] wartosci = new int[dziedzina.length];
		
		for (int i = 0; i < dziedzina.length; i++) {
			wartosci[i] = f.aplikuj(dziedzina[i]);
		}
		
		this.wartosci = wartosci;
	}
	
	@Override
	public int[] dziedzina() {
		return dziedzina;
	}
	
	@Override
	public int aplikuj(int x) {
		if (ostatni != -1 && ostatni != dziedzina.length -1) {
			if (dziedzina[ostatni + 1] == x) {
				ostatni++;
				return wartosci[ostatni];
			}
		}
		
		int indeks = Arrays.binarySearch(dziedzina, x);
		
		if (indeks < 0) {
			return 0;
		}
		
		ostatni = indeks;
		return wartosci[indeks];
	}
}
