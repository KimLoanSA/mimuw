import java.util.Random;

public class TurystaSkoncentrowany extends Turysta {
	private Miasto ulubione;
	
	public TurystaSkoncentrowany(Miasto rodzinneMiasto, Miasto ulubione) {
		super(rodzinneMiasto);
		
		this.ulubione = ulubione;
	}

	@Override
	public Miasto wybierzMiasto(Mapa[] miasta) {
		int dlugosc = miasta.length;
		Random gen = new Random();
		
		if (aktualneMiasto == ulubione) {
			return ulubione;
		}
		
		for (Mapa akt : miasta) {
			if (akt.miasto() == ulubione) {
				return ulubione;
			}
		}
		
		return miasta[gen.nextInt(dlugosc)].miasto();
	}
}
