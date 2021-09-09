
public class TurystaAgroturysta extends Turysta {
	
	public TurystaAgroturysta(Miasto rodzinneMiasto) {
		super(rodzinneMiasto);
	}

	@Override
	public Miasto wybierzMiasto(Mapa[] miasta) {
		int mieszkancy = miasta[0].miasto().liczbaMieszkancow();
		
		for (Mapa akt : miasta) {
			if (akt.miasto().liczbaMieszkancow() < mieszkancy) {
				mieszkancy = akt.miasto().liczbaMieszkancow();
			}
		}
		
		int rok = 1000000000;
		
		for (Mapa akt : miasta) {
			if (akt.miasto().liczbaMieszkancow() == mieszkancy) {
				if (rok > akt.miasto().rok()) {
					rok = akt.miasto().rok();
				}
			}
		}
		
		for (Mapa akt : miasta) {
			if (akt.miasto().liczbaMieszkancow() == mieszkancy &&
					akt.miasto().rok() == rok) {
				return akt.miasto();
			}
		}
		
		return miasta[0].miasto();
	}
}
