
public class UczestnikHistoryk extends Uczestnik {
	private int poczatekRok;
	private int koniecRok;
	
	public UczestnikHistoryk(int budzet, String pseudonim, int poczatekRok, int koniecRok) {
		super(budzet, pseudonim);
		
		this.poczatekRok = poczatekRok;
		this.koniecRok = koniecRok;
	}

	@Override
	public boolean czyChcesz(Przedmiot przedmiot) {
		if (przedmiot.rok() <= koniecRok && przedmiot.rok() >= poczatekRok && przedmiot.cena() <= budzet)
			return true;
		
		return false;
	}
}