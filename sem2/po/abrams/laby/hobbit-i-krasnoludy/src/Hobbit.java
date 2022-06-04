import java.util.LinkedList;

public class Hobbit {
	
	public void rozdziel(Skarb[] skarby, Krasnolud[] krasnoludy, Worek[] worki) {
		boolean[] czyjWorek = new boolean[krasnoludy.length];
		int indeksS = 0;
		
		for (int i = 0; indeksS < skarby.length && i < krasnoludy.length; i++) {
			while (krasnoludy[i].czyChce(worki[i].pojemnosc())) {
				czyjWorek[i] = worki[i].dodaj(skarby[indeksS]);
				krasnoludy[i].wezInfo(skarby[indeksS]);
				
				indeksS++;
			}
		}
		
		
		for (int i = 0; i < krasnoludy.length; i++) {
			System.out.print(krasnoludy[i] + ": ");
			
			if (czyjWorek[i]) {
				worki[i].wypisz();
			}
			
			System.out.println();
		}
		
		System.out.print("Worek Bilba:");
		
		for (int i = 0; i < krasnoludy.length; i++) {
			if (czyjWorek[i] == false) {
				worki[i].wypisz();
			}
		}
		
		
	}
}
