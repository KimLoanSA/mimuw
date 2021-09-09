
public class Statek {
	private int dukaty;
	private int pojemnosc;
	private int zawartosc;
	private int liczbaPestek;
	private Wyspa[] plan;
	
	public Statek(int pojemnosc, Wyspa[] plan) {
		this.pojemnosc = pojemnosc;
		this.plan = plan;
		this.dukaty = 1000;
		this.liczbaPestek = 0;
		this.zawartosc = 0;
	}
	
	public Jablko wybierzJablko(Jablko[] jablka) {
		Jablko wynik = null;
		
		if (zawartosc < pojemnosc) {
			int maxCena = 0;
			
			for (Jablko akt : jablka) {
				if (akt.pestki() * 40 <= dukaty && maxCena < akt.pestki() * 40) {
					maxCena = akt.pestki() * 40;
					wynik = akt;
				}
			}
		}
		
		return wynik;
	}
	
	public void dzien(int numerDnia) {
		
		if (plan[numerDnia] != null) {
			Jablko wybraneJablko = wybierzJablko(plan[numerDnia].jablonie());
			
			if (wybraneJablko != null) {
				plan[numerDnia].usunJablko(wybraneJablko);
 				dukaty -= wybraneJablko.pestki() * 40;
				liczbaPestek += wybraneJablko.pestki();
				zawartosc++;
			}
		}
	}
	
	public int liczbaPestek() {
		return liczbaPestek;
	}
}
