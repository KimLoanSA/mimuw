import java.util.Random;

public class TurystaLosowy extends Turysta {
	
	public TurystaLosowy(Miasto rodzinneMiasto) {
		super(rodzinneMiasto);
	}
	
	@Override
	public Miasto wybierzMiasto(Mapa[] miasta) {
		int rozmiar = miasta.length;
		Random gen = new Random();
		
		return miasta[gen.nextInt(rozmiar)].miasto();
	}

}
