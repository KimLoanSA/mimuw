import java.util.Random;

public class UczestnikLosowy extends Uczestnik {
	
	public UczestnikLosowy(int budzet, String pseudonim) {
		super(budzet, pseudonim);
	}

	@Override
	public boolean czyChcesz(Przedmiot przedmiot) {
		Random gen = new Random();
		
		if (gen.nextBoolean() == true && przedmiot.cena() <= budzet)
			return true;
		
		return false;
	}
	
	
}
