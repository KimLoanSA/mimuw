
public abstract class Funkcja {
	public abstract int[] dziedzina();
	public abstract int aplikuj(int x);
	
	
	public int masa() {
		int wynik = 0;
		int[] dziedzina = dziedzina();
		
		for (int i : dziedzina) {
			wynik += aplikuj(i);
		}
		
		return wynik;
	}
}
