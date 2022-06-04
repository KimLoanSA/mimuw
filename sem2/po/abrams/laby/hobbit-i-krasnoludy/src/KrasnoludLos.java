import java.util.Random;

public class KrasnoludLos extends Krasnolud {
	@Override
	public boolean czyChce(int pojemnoscWorka) {
		Random gen = new Random();
		
		return gen.nextBoolean();
	}

	@Override
	public void wezInfo(Skarb s) {
		//nic sie nie dzieje, czy takie przypadki jakos da sie obslugiwac?
	}
	
}
