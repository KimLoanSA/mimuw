
public class KrasnoludProcent extends Krasnolud {
	private int procent;
	private int objetoscSkarbow;
	
	public KrasnoludProcent() {
		this.procent = 50;
	}

	public KrasnoludProcent(int procent) {
		this.procent = procent;
	}
	
	@Override
	public boolean czyChce(int pojemnoscWorka) {
		if (objetoscSkarbow * 100 <= pojemnoscWorka * procent) {
			return true;
		}
		
		return false;
	}

	@Override
	public void wezInfo(Skarb s) {
		objetoscSkarbow += s.rozmiar();
	}
}
