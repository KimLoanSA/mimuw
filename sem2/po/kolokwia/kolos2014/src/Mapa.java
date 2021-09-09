
public class Mapa {
	private int odlegosc;
	private Miasto miasto;

	public Mapa(Miasto miasto, int odleglosc) {
		this.miasto = miasto;
		this.odlegosc = odleglosc;
	}
	
	public int odlegosc() {
		return odlegosc;
	}
	
	public Miasto miasto() {
		return miasto;
	}
}
