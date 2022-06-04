
public class UczestnikOszczędny extends Uczestnik {
	private int liczba;
	private int suma;

	public UczestnikOszczędny(int budzet, String pseudonim) {
		super(budzet, pseudonim);
	}
	
	@Override
	public boolean czyChcesz(Przedmiot przedmiot) {
		if (liczba == 0) {
			liczba++;
			suma += przedmiot.cena();
			
			return false;
		}
		
		
		if (przedmiot.cena() <= budzet && przedmiot.cena() < (suma / liczba)) {
			liczba++;
			suma += przedmiot.cena();
			
			return true;
		}
		
		liczba++;
		suma += przedmiot.cena();
		
		return false;
	}
}
