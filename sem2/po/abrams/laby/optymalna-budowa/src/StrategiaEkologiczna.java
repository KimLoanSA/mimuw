
public class StrategiaEkologiczna extends Strategia {
	private int rozmiarProjektu;
	private long[] najmniejszyOdpadDlaMaski;
	private long[] dp;
	private int[] zapisMaski;
	private RekordRaportu[] odtwarzanieWyniku;
	
	@Override
	public void rozwiaz(Cennik cennik, Projekt projekt) {
		rozmiarProjektu = projekt.rozmiar();
		najmniejszyOdpadDlaMaski = new long[(1 << rozmiarProjektu)];
		odtwarzanieWyniku = new RekordRaportu[(1 << rozmiarProjektu)];
		dp = new long[(1 << rozmiarProjektu)];
		zapisMaski = new int[(1 << rozmiarProjektu)];
		
		//preprocessing
		for (int maska = 0; maska < (1 << rozmiarProjektu); maska++) {
			long dlugoscPretow = 0;
			dp[maska] = -1;
			
			for (int j = 0; j < rozmiarProjektu; j++) {
				if ((maska & (1 << j)) != 0) {
					dlugoscPretow += projekt.pretNr(j).dlugosc();
				}
			}
			
			RekordCennika aktualnyRekord = cennik.znajdzZNajmniejszymOdpadem(new Pret(dlugoscPretow));
			najmniejszyOdpadDlaMaski[maska] = aktualnyRekord.pret().dlugosc() - dlugoscPretow;
			odtwarzanieWyniku[maska] = new RekordRaportu(aktualnyRekord);
			
			for (int j = 0; j < rozmiarProjektu; j++) {
				if ((maska & (1 << j)) != 0) {
					odtwarzanieWyniku[maska].dodajCzesc(projekt.pretNr(j));
				}
			}
		}
		
		dp[0] = 0;
		//liczenie dp
		for (int maska = 0; maska < (1 << rozmiarProjektu); maska++) {
			for (int podzb = maska; podzb > 0; podzb = (podzb - 1) & maska) {
				if (najmniejszyOdpadDlaMaski[podzb] >= 0 ) {
					if (((dp[maska ^ podzb] + najmniejszyOdpadDlaMaski[podzb]) < dp[maska]) || dp[maska] == -1) {
						dp[maska] = dp[maska ^ podzb] + najmniejszyOdpadDlaMaski[podzb];
						zapisMaski[maska] = podzb; 
					}
				}
			}
		}
	}

	@Override
	public void wypisz() {
		Raport raport = new Raport();
		
		for (int maska = (1 << rozmiarProjektu) - 1; maska > 0; maska = zapisMaski[maska] ^ maska) {
			raport.dodajCalyRekord(odtwarzanieWyniku[zapisMaski[maska]]);
		}
		
		raport.wypisz();	
	}
	
}
