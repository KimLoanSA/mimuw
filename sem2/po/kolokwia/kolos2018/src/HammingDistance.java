
public class HammingDistance implements IMetryka {
	public int odleglosc(Funkcja f, Funkcja g) {
		int[] dziedzinaF = f.dziedzina();
		int[] dziedzinaG = g.dziedzina();
		
		if (dziedzinaF.length != dziedzinaG.length) {
			return -1;
		}
		
		for (int i = 0; i < dziedzinaF.length; i++) {
			if (dziedzinaF[i] != dziedzinaG[i]) {
				return -1;
			}
		}
		
		int wynik = 0;
		
		for (int i = 0; i < dziedzinaF.length; i++) {
			wynik += Math.abs(f.aplikuj(dziedzinaF[i]) - g.aplikuj(dziedzinaG[i]));
		}
		
		return wynik;
	} 
}
