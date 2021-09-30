# Uruchamianie

ABY TO DZIALALO TRZBA MIEC KLUCZ RSA DO STUDENTSA!!

w pliku build_and_run_students.sh trzeba pozmieniac pole indeks NA SWOJ NUMER INDEKSU i opcjonalnie sciezki jesli jest inna struktura plikow
```
chmod +x build/*
cd build/bash
./build_and_run_students.sh
```

FLAGI
- `[-c]` - czysci wszystko po sobie
- `[-b]` - tylko buduje
- `[-t]` - brak kopiowania tesow

w przeciwnym przypadku buduje i odpala bez czyszczenia

## Uruchamianie lokalnie

```
./build.sh <sciezka do folderu z plikiem.asm> <sciezka do folderu z plikiem .c i .h> <sciezka do folderu gdzie ma trafic zbudowane>
./run_tests.sh <sciezka do folderu gdzie ma trafic zbudowane (3. argument z build.sh)>
```

# Uzywanie
Trzeba zaimplementowac sygnatury funckji podanych w `pix.h`.
Warto zeby przestrzegaly standardu ABI!
```
uint64_t modPix(uint64_t a, uint64_t mod);
uint64_t powPix(uint64_t a, uint64_t pow, uint64_t mod);
uint64_t sum1Pix(uint64_t n, uint64_t j);
uint64_t sum2Pix(uint64_t n, uint64_t j);
uint64_t pixPi(uint64_t n);
void pwPix(uint32_t *ppi, uint64_t *pidx, uint64_t max);
```

- `uint64_t modPix(uint64_t a, uint64_t mod)`
funkcja ma zwracac a % mod

- `uint64_t powPix(uint64_t a, uint64_t pow, uint64_t mod)`
funkcja ma zwracac (a^pow) % mod (algorytm szybkiego potegowania)

- `uint64_t sum1Pix(uint64_t n, uint64_t j)`
funkcja ma zwracac wartosc pierwszej sumki z dolu strony (https://math.stackexchange.com/questions/880904/how-do-you-use-the-bbp-formula-to-calculate-the-nth-digit-of-π)
wymagana dokladnosc do 2^(-32)

- `uint64_t sum2Pix(uint64_t n, uint64_t j)`
funkcja ma zwracac wartosc drugiej sumki z dolu strony (https://math.stackexchange.com/questions/880904/how-do-you-use-the-bbp-formula-to-calculate-the-nth-digit-of-π)
wymagana dokladnosc do 2^(-32)

- `uint64_t pixPi(uint64_t n)`
ma zwracac n-ta liczbe pi (n w tym zworku https://math.stackexchange.com/questions/880904/how-do-you-use-the-bbp-formula-to-calculate-the-nth-digit-of-π)

- `void pwPix(uint32_t *ppi, uint64_t *pidx, uint64_t max)`
funckja sprawdza poprawnosc uzycia mutexa, dzialac ma jak z tresci, w podanej tablicy trzeba zrobic na podanym indeksie + 1
