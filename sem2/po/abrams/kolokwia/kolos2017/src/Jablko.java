import java.util.Random;

public class Jablko {
	private int pestki;
	
	public Jablko() {
		Random gen = new Random();
		
		this.pestki = gen.nextInt(6) + 2;
	}
	
	public int pestki() {
		return pestki;
	}
}
