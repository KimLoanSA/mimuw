import java.util.Arrays;
import java.util.Random;

public class Wyspa {
	private Jablko[] jablonie;
	private int liczbaJabloni;
	
	public Wyspa() {
		Random gen = new Random();
		
		this.liczbaJabloni = gen.nextInt(7);
		jablonie = new Jablko[liczbaJabloni];
		
		for (int i = 0; i < liczbaJabloni; i++) {
			jablonie[i] = new Jablko();
		}
	}
	
	public Jablko[] jablonie() {
		return jablonie;
	}
	
	public void usunJablko(Jablko jablko) {
		int indeks = 0;
		
		for (int i = 0; i < liczbaJabloni; i++) {
			if (jablonie[i] == jablko) {
				indeks = i;
			}
		}
		
		Jablko pom = jablonie[indeks];
		jablonie[indeks] = jablonie[liczbaJabloni - 1];
		jablonie[liczbaJabloni - 1] = pom;
		liczbaJabloni--;
		
		jablonie = Arrays.copyOf(jablonie, liczbaJabloni);	
	}
}
