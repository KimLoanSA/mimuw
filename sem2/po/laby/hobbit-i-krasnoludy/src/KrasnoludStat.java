
public class KrasnoludStat extends Krasnolud {
	private int objetoscSkarbow;
	private int liczbaSkarbow;
	
	@Override
	public void wezInfo(Skarb s) {
		objetoscSkarbow += s.rozmiar();
		liczbaSkarbow++;
	}
	
	@Override
	public boolean czyChce(int pojemnoscWorka) {
		if (liczbaSkarbow == 0) {
			return true;
		}
		
		int srednia = objetoscSkarbow / liczbaSkarbow;
		
		if (objetoscSkarbow + srednia <= pojemnoscWorka) {
			return true;
		}
		
		return false;
	}
}
