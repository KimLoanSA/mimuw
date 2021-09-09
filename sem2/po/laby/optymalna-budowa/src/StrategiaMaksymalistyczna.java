
public class StrategiaMaksymalistyczna extends Strategia {
	private boolean[] uzyte;
	private Raport raport = new Raport();
	
	@Override
	public void rozwiaz(Cennik cennik, Projekt projekt) {
		int rozmiarProjektu = projekt.rozmiar();
		uzyte = new boolean[rozmiarProjektu];
		
		for (int i = rozmiarProjektu - 1; i >= 0; i--) {
			if (uzyte[i] == false) {
				RekordCennika aktualnyRekord = cennik.dajNajwiekszy();
				raport.nowyRekord(aktualnyRekord);
				
				for (int j = i; j >= 0; j--) {
					if (uzyte[j] == false && raport.czyDaSieDodacDoOstatniegoRekordu(projekt.pretNr(j))) {
						raport.dodajDoOstatniegoRekordu(projekt.pretNr(j));
						uzyte[j] = true;
					}
				}	
			}
		}
	}

	@Override
	public void wypisz() {
		raport.wypisz();
	}
}
