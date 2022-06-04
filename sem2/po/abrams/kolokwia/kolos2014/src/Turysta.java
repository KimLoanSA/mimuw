
public abstract class Turysta {
	protected Miasto aktualneMiasto;
	private Miasto rodzinneMiasto;
	
	public Turysta(Miasto rodzinneMiasto) {
		this.rodzinneMiasto = rodzinneMiasto;
		this.aktualneMiasto = rodzinneMiasto;
	}
	
	public void wrocDoRodzinnego() {
		aktualneMiasto = rodzinneMiasto;
	}
	
	public void idzDoMiasta(Miasto miasto) {
		aktualneMiasto = miasto;
	}
	
	
	public abstract Miasto wybierzMiasto(Mapa[] miasta);
}
