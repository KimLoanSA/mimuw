
public class Miasto {
	private String nazwa;
	private int rok;
	private int liczbaMieszkancow;
	
	public Miasto(String nazwa, int rok, int liczbaMieszkancow) {
		this.nazwa = nazwa;
		this.rok = rok;
		this.liczbaMieszkancow = liczbaMieszkancow;
	}
	
	public String nazwa() {
		return nazwa;
	}
	
	public int rok() {
		return rok;
	}
	
	public int liczbaMieszkancow() {
		return liczbaMieszkancow;
	}
	
	public void zwiedz(Turysta turysta) {
		
	}
}
