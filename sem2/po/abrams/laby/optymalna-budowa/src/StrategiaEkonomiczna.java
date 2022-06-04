
public class StrategiaEkonomiczna extends Strategia {
	private int rozmiarProjektu;
	private long[] najmniejszaCenaDlaMaski;
	private long[] dp;
	private int[] zapisMaski;
	private RekordRaportu[] odtwarzanieWyniku;
		
	@Override
	public void rozwiaz(Cennik cennik, Projekt projekt) {
		rozmiarProjektu = projekt.rozmiar();
		najmniejszaCenaDlaMaski = new long[(1 << rozmiarProjektu)];
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
			
			RekordCennika aktualnyRekord = cennik.znajdzZNajmniejszaCena(new Pret(dlugoscPretow));
			najmniejszaCenaDlaMaski[maska] = aktualnyRekord.cena();
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
				if (najmniejszaCenaDlaMaski[podzb] >= 0) {
					if ((dp[maska ^ podzb] + najmniejszaCenaDlaMaski[podzb] < dp[maska]) || dp[maska] == -1) {
						dp[maska] = dp[maska ^ podzb] + najmniejszaCenaDlaMaski[podzb];
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
