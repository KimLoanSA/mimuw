# MIMUW-S4-SIK-TestHttp-Tests
Testy jednostkowo-integracyjne do zadania z siku TestHttp-tests, raczej dla ludzi ktorzy dopiero zaczeli, albo chca zaczac i nie wiedza od czego to klepac.

# Ogolnie
Sa to testy, ktore testuja poszczegolne czesci rozwiazania. Uzycie ich pewnie wymaga odrobinke poswiecenia, zeby zaimplementowac interface'y i poprawic jakies makefile, jednak jak sie juz przez to przebrnie to kodzenie jest raczej przyjemniejsze i daje jakas namiastke TDD. Co do formy itp, to nie umiem C i nie chce umiec C, jest to zbitka jakis moich pomyslow, z braku frameworkow zrobilem to tak, a nie inaczej, jesli sie zrobi dokladnie to co napisze to raczej nie ma potrzeby zaglebiac sie w szczegoly rozwiazania. Z racji ze mam macbooka i w uklonie do innych posiadaczy odpala to sie wyszstko na obrazie dockerowym z systemem takim co posiada students. <b>Tak, macbook i tak jest fajny, to C jest niefajne.</b>

Docker sie sam buduje i wszystko sb ladnie instaluje, czysci smieci, wiec uwazajcie na swoje stare obrazy, ktore nie maja nazwy i tagow. Buduje 2 obrazy, zeby pozniej bylo szybciej, jak bede robil update'a tego to mozeliwe ze bede podbijal taga base obrazu, wiec po oddaniu zadania moze warto poczyscic obrazy

Kodze w javie, wiec podzial na foldery itp jest raczej taki jaki sie robi w kodach JVM.

# Wymagania

- Posiadanie godnosci czlowieka, zeby nie testowac gdb (jesli musisz tak debugowac program, ktory dopiero co napisales to znaczy ze zepsules w developerce - mysl na wieczor)
- Docker na kompie (fajna rzecz, jak ktos nie ma to polecam, przyda sie)
- Chec zaklepania tego zadania


# Jak tego uzywac?
## Zaimplementuj interface'y z `docker-test/src/test/c/interfaces/`
Jak juz mowilem nie umiem w c i nie chce umiec, wiec testy raczej maja forme implementacji jakis interface'ow, moze przynajmniej zmusi to kogos do podzialu rozwiazania na jakies moduly.

Powinny one zostac zaimplementowane oczywiscie w `solve/src/test/c/`, includowac moj interface i go implementowac.

## Popraw `makefile` w `solve/`
Powinien on budowac cale rozwiazanie (`testhttp_raw`) i obslugiwac `make clean` i pewnie jakies moduly uzywane w rozwiazaniu (do `.o`), ktore pozniej wlasnie mozna dolaczyc do tego ostatniego `makefile`

## Popraw `makefile` w `solve/src/test/`
Powinien budowac test do `.o` i obslugiwac `make clean`
(mozna sie na moim przykladzie wzorowac)

## Popraw `makefile` w `docker-test/`
Ma on budowac juz test, w nim trzeba, jesli sie ma (powinno sie miec xd) jakies moduly ktorych uzywacie w implementacji tego testu
(mozna sie na moim przykladzie wzorowac)

# Uruchamianie
wystarczy:
```
./runDockerTests.sh
```

Pierwsze odpalenie bedzie sie dluzej odpalalo, bo bedzie budowalo obraz z potrzebnymi bibliotekami, pozniejsze juz nie beda musialy tego robic (chyba ze podbije wersje) i powinny szybko sie odpalac.

## Jesli uzywasz dziwnych bibliotek:
Aktualnie zainstalowane na obrazie sa:
```
gcc
make
glibc-devel
diffutils
```

Jesli chcesz inne to wystarczy dodac w `docker-test/Dockerfile`
```
RUN poldek --shcmd="install <nazwa biblioteki>"
```

i w `docker-test/build/buildBaseDockerImage.sh` podbic pole
```
BASE_IMAGE_VERSION="1_0"
```
np na
```
BASE_IMAGE_VERSION="1_1"
```

# Testy:
## wersja 1.0:
<b>Base image tag: 1_0</b>

<b>Test image tag: 1_0</b>
### Testy na walidacje liczby argumentow programu
Plik testowy to: 
`docker-test/src/test/bash/program_arguments_validation_test`

Wymagane jest jedynie wywalenie sie z kodem 1 (bo tak bylo na labach w err.h) jesli liczba arguemntow jest zla.

Przyklad wywolania:
`./testhttp_raw www.mimuw.edu.pl:80 ciasteczka.txt` (powinien sie wywalic)

### Testy na parsowanie responsa z serwera z kodem innym niz `200 OK`
Inteface: 
`docker-test/src/test/c/interfaces/response_resolver_test.h`

Medota:
```
void report_for_response_test(char **response_parts, size_t number_of_response_parts, size_t response_part_size);
```

- `char **response_parts` tablica stringow z kawalkami odpowiedzi
- `size_t number_of_response_parts` liczba tych kawalkow
- `size_t response_part_size` rozmiar pojedynczego kawalka (w sumie nie polecam sie zapinac jakos na sztywno z tym)

Medota symuluje dostawanie responsa w kawalkach ktore bedzie zwracal read na polaczeniu, powinna po prostu wypisac raport zgodnie z trescia zadania 
```
Jeśli odpowiedź serwera jest inna niż 200 OK (np. 202 Accepted) klient ma podać raport w postaci zawartości wiersza statusu uzyskanej odpowiedzi.
```
rozumiem ze to jest np:
```
HTTP/1.1 404 Not Found
```
### wersja 1.1:
Headery koncza sie CRLF i dodanie id testu w raporcie.

## wersja 2.0:
<b>Base image tag: 1_0</b>

<b>Test image tag: 2_0</b>

### Testy na parsowanie responsa z serwera z kodem `200 OK`
Inteface: 
`docker-test/src/test/c/interfaces/response_resolver_test.h`

(taki sam jak wyzej)
Medota:
```
void report_for_response_test(char **response_parts, size_t number_of_response_parts, size_t response_part_size);
```

- `char **response_parts` tablica stringow z kawalkami odpowiedzi
- `size_t number_of_response_parts` liczba tych kawalkow
- `size_t response_part_size` rozmiar pojedynczego kawalka (w sumie nie polecam sie zapinac jakos na sztywno z tym)

(taka sama jak wyzej)

Medota symuluje dostawanie responsa w kawalkach ktore bedzie zwracal read na polaczeniu, powinna po prostu wypisac raport zgodnie z trescia zadania 

## wersja 3.0:
<b>Base image tag: 1_0</b>

<b>Test image tag: 3_0</b>

### Testy na wysylanie requesta 
Inteface: 
`docker-test/src/test/c/interfaces/request_resolver_test.h`

Medota:
```
void send_request_test(const char *address, const char *file_name);
```

- `const char *address` string z adresem (argv[3])
- `const char *file_name` nazwa pliku z cookies do wyslania

Medota symuluje dostawanie wysylanie requesta, wystraczy wypisac na standardowe wyjscie requesta (w fprintf podmienic `File *` na `stdout`

zakladam ze powinny byc headery:
```
Host: <host>
User-Agent: testhttp_raw
Connection: close
Cookie: key=value
...
```

### Testy na argumenty programu sprawdzaja istnienie pliku z cookies i testuja walidowanie portu
Takie jak wyzej, wiecej przypadkow na podstawie forum

## wersja 4.0
<b>Base image tag: 1_0</b>

<b>Test image tag: 3_0</b>


Nowe testy do requesta, ciastka tylko w jednej linii oraz znaki # i ? w urlu
