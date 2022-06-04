
public class Licytator {
	public Uczestnik[] przeprowadz(Przedmiot[] przedmioty, Uczestnik[] uczestnicy) {
		Uczestnik[] wynik = new Uczestnik[przedmioty.length];
		
		
		for (int i = 0; i < przedmioty.length; i++) {
			Uczestnik aktualny = null;
			boolean ok = false;
			
			for (int j = 0; j < uczestnicy.length; j++) {
				if (ok) {
					uczestnicy[j - 1] = uczestnicy[j];
					
					if (j == uczestnicy.length - 1)
						uczestnicy[j] = aktualny; 
				}
				else {
					if (uczestnicy[j].czyChcesz(przedmioty[j])) {
						ok = true;
						aktualny = uczestnicy[j];
					}
				}
			}
			
			wynik[i] = aktualny;
		}
		
		return wynik;
	}
}
