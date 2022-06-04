
public class UczestnikSentymentalny extends Uczestnik {
	private String kraj;
	
	public UczestnikSentymentalny(int budzet, String pseudonim, String kraj) {
		super(budzet, pseudonim);
		this.kraj = kraj;
	}
	
	@Override
	public boolean czyChcesz(Przedmiot przedmiot) {
		if (przedmiot.kraj() == kraj && przedmiot.cena() <= budzet)
			return true;
		
		return false;
	}
	
}
