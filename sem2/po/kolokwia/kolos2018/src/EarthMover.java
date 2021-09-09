
public class EarthMover implements IMetryka {

	@Override
	public int odleglosc(Funkcja f, Funkcja g) {
		int masaF = f.masa();
		int masaG = g.masa();
		int[] dziedzinaF = f.dziedzina();
		int[] dziedzinaG = g.dziedzina();
		
		
		if (masaF != masaG || dziedzinaF.length != dziedzinaG.length) {
			return -1;
		}
		
		int wynik = 0;
		int kredyt = 0;
		
		for (int i = 0; i < dziedzinaF.length; i++) {
			int wartF = f.aplikuj(dziedzinaF[i]);
			int wartG = g.aplikuj(dziedzinaG[i]);
			
			wynik += Math.abs(kredyt);
			kredyt += wartF - wartG;
		}
		
		return wynik;
	}
}
