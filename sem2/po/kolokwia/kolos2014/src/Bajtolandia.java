
public class Bajtolandia {
	private Miasto[] miasta;
	private Mapa[] mapa;
	
	public Bajtolandia(Miasto[] miasta, Mapa[] mapa) {
		this.miasta = miasta;
		this.mapa = mapa;
	}
	
	public void przeprowadzMajowke(Turysta[] turysci, Mapa[] mapa) {
		
		for (int dzien = 0; dzien < 7; dzien++) {
			for (Turysta akt : turysci) {
				Miasto wybrane = akt.wybierzMiasto(mapa);
				akt.idzDoMiasta(wybrane);
				wybrane.zwiedz(akt);
			}
		}
		
		
		for (Turysta aktT : turysci) {
			aktT.wrocDoRodzinnego();
		}
	}
}
