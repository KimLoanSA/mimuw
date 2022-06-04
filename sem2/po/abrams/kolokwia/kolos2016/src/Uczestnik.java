
public abstract class Uczestnik {
	protected int budzet;
	private String pseudonim;
	
	public Uczestnik(int budzet, String pseudonim) {
		this.budzet = budzet;
		this.pseudonim = pseudonim;
	}
	
	public String pseudonim() {
		return pseudonim;
	}
	
	public abstract boolean czyChcesz(Przedmiot przedmiot);
}
