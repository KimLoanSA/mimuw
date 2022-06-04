# ODPALANIE
## ABY TO DZIALALO TRZBA MIEC KLUCZ RSA DO STUDENTSA!!

w pliku `build_and_run_students.sh` trzeba pozmieniac pole `indeks` NA SWOJ NUMER INDEKSU i opcjonalnie sciezki jesli jest inna struktura plikow

```
chmod +x build/*
cd build/bash
./build_and_run_students.sh
```

# FLAGI
- `[-c]` - czysci wszystko po sobie
- `[-b]` - tylko buduje
- `[-t]` - brak kopiowania tesow

<b>w przeciwnym przypadku buduje i odpala bez czyszczenia</b>

# TESTY

Moje testy sprawdzaja tylko walidacje parametrow / wejscia, czyli:
- liczbe parametrow
- dlugosc parametrow
- poprawnosc znakow
- czy permutacja jest permutacja 
- czy T jest permutacja 21 cykli

Oznacza to ze sprawdzany jest TYLKO KOD WYJSCIA!!

TESTY POPRAWNOSCIOWE to sa testy pythonowe
