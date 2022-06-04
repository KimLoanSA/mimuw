import java.util.Arrays;

public class TurystaCiekawy extends Turysta {
	private Miasto[] odwiedzone = new Miasto[3];
	private int indeks = -1;
	
	public TurystaCiekawy(Miasto rodzinneMiasto) {
		super(rodzinneMiasto);
	}
	
	@Override
	public Miasto wybierzMiasto(Mapa[] miasta) {
		if (indeks == -1) {
			indeks++;
			odwiedzone[indeks] = miasta[indeks].miasto();
			
			return miasta[indeks].miasto();
		}
		
		for (Mapa aktualne : miasta) {
			boolean ok = true;
			
			for (int i = 0; i <= indeks; i++) {
				if (odwiedzone[i] == aktualne.miasto()) {
					ok = false;
				}
			}
			
			if (ok) {
				indeks++;
				
				if (odwiedzone.length == indeks) {
					odwiedzone = Arrays.copyOf(odwiedzone, odwiedzone.length * 2);
				}
				
				odwiedzone[indeks] = aktualne.miasto();
				
				return aktualne.miasto();
			}
		}
		
		int maxDlugosc = 0;
		
		for (Mapa aktualne : miasta) {
			if (aktualne.odlegosc() > maxDlugosc)
				maxDlugosc = aktualne.odlegosc();
		}
		
		for (Mapa aktualne : miasta) {
			if (aktualne.odlegosc() == maxDlugosc)
				return aktualne.miasto();
		}
		
		return miasta[0].miasto();
	}	

}
