/** @file
 * Interface klasy reprezentujacej miasta i drogi je laczace
 */

#ifndef DROGI_CITIESANDROADS_H
#define DROGI_CITIESANDROADS_H


/** @brief Struktura reprezentujaca miasto.
 * Kazda miasto ma nazwe, id,
 * strukture trzymajaca sasiednie miasta,
 * strukture trzymajaca informacje o nim,
 * bitset trzymajacay drogi krajowe do ktorych nalezy
 */
typedef struct City City;

/** @brief Struktura reprezentujaca droge.
 * Kazda droga ma dlugosc, rok budowy/naprawy,
 * i miasta, ktore laczy
 */
typedef struct  Road Road;

/** @brief Funkcja tworzaca nowe miasto.
 * Funkcja alkouje pamiec i zwraca wskaznik na nowe miasto
 * @param[in] argName - nazwa powstajacego miasta
 * @param[in,out] argGen - generator id
 * @return Wskaznik na nowe miasto
 */
City *createCity(const char *argName, RandomIdGen *argGen);

/** @brief Funkcja zwracajaca nazwe miasta.
 * @param[in] argCity - dane miasto
 * @return Nazwa danego miasta
 */
const char *getCityName(City *argCity);

/** @brief Funkcja usuwajaca miasto.
 *  Funckja usuwa z pamieci cala strukture i jej skladowe
 * @param[in,out] argCity - usuwane miasto
 */
void deleteCity(City **argCity);


/** @brief Funkcja tworzaca nowa droge.
 * @param[in] argLength - dlugosc nowej drogi
 * @param[in] argCity1 - miasto nr 1, ktore laczy droga
 * @param[in] argCity2 - miasto nr 2, ktore laczy droga
 * @param[in] argYear - rok budowy drogi
 * @return Wskaznik na nowa droge
 */
Road *createRoad(unsigned argLength, City *argCity1, City *argCity2, int argYear);

/** @brief Funcja usuwajaca droge.
 * Funkcja usuwa z pamieci cala strukture i jej skladowe
 * @param[in,out] argRoad - usuwana droga
 */
void deleteRoad(Road **argRoad);

/** @brief Funkcja resetuje informacje o miescie.
 * @param[in] argCity - dane miasto
 */
void resetCityInfo(City *argCity);

/** Funkcja ustawiajaca odleglosc miasta od zrodla.
 * @param[in] argDist - odlaglosc miasta od zrodla
 * @param[in,out] argCity - dane miasto
 */
void setCityDistance(City *argCity, uint64_t argDist);

/** @brief Funkcja zwracajaca odleglosc miasta od zrodla.
 * @param[in] argCity - dane miasto
 * @return Odleglosc miasta od zrodla
 */
uint64_t getCityDistance(City *argCity);

/** @brief Funckja zwracajaca rok budowy najstarszej z drog.
 * @param[in] argCity - dane miasto
 * @return Rok budowy/remontu najstarszej z drog
 */
int getCityYear(City *argCity);

/** @brief Funckja ustawia poprzednika dla miasta.
 * @param[in,out] argCity - dane miasto
 * @param[in] argCityPrev - miasto bedace poprzednikiem
 */
void setCityPrev(City *argCity, City *argCityPrev);

/** @brief Funckja zwraca poprzednika miasta
 * @param[in] argCity - dane miasto
 * @return Wskaznik na poprzednika danego miasta
 */
City *getCityPrev(City *argCity);

/** @brief Funckja zwraca droge miedzy miastami.
 * Funkcja zwraca droge miedzy miastami,
 * jesli nie istenieje to zwraca null
 * @param[in] argCity1 - miasto nr 1
 * @param[in] argCity2 - miasto nr 2
 * @return Wskaznik na droga miedzy miastem nr 1 i miastem nr 2, NULL jesli nie istnieje
 */
Road *findRoad(City *argCity1, City *argCity2);

/** @brief Funkcja dodaje droge miedzy miastami.
 * @param[in] argCity1 - miasto nr 1
 * @param[in] argCity2 - miasto nr 2
 * @param[in] lenght - dlugosc dodawanej drogi
 * @param[in] year - rok budowy dodawanej drogi
 * @return @p true, jesli dodawanie sie udalo, @p false wpp
 */
bool addRoadCity(City *argCity1, City *argCity2, unsigned lenght, int year);

/** @brief Funkcja dodajaca konkretna droge miedzy miastami.
 * @param[in,out] argCity1 - miasto nr 1
 * @param[in,out] argCity2 - miasto nr 2
 * @param[in] tempRoad - dodawana droga
 * @return @p true jesli sie dodawanie udalo, @p false wpp
 */
bool addRoadCityElem(City *argCity1, City *argCity2, Road *tempRoad);

/** @brief Funkcja sprawdza czy droga do miasta jest jednoznaczna.
 * Funkcja sprawdza czy dane miasto ma jednoznaczeni wyznaczona
 * najktorsza sciezke, tj najstarsze odcinki nie sa w tym samym wieku.
 * @param[in] argCity - dane miasto
 * @return @p true, jesli sciezka jest jednoznacznie wyznaczona, @p false wpp
 */
bool isGoodCity(City *argCity);

/** @brief Funkcja zmienia rok budowy drogi.
 * Funkcja ustawia rok budowy/remontu drogi,
 * jesli podana data jest wczesniejsza
 * niz byla to zwraca false
 * @param[in] argRoad - edytowana droga
 * @param[in] argYear - nowy rok remontu
 * @return @p true, jesli operacja sie powiodla, @p false wpp
 */
bool setRoadYear(Road *argRoad, int argYear);

/** @brief Struktura kolejki
 */
typedef struct Queue Queue;

/** @brief Funckja tworzaca nowa kolejke.
 * Funkcja alkoje pamiec i tworzy pusta kolejke,
 * zwraca NULL jesli alokacja sie nie powiodla
 * @return Wskaznik na nowa kolejke, NULL jesli sie alokowanie nie udalo
 */
Queue *createQueue();

/** @brief Funkcja dodajaca do kolejki miasto.
 * @param[in,out] argQueue - edytowana kolejka
 * @param[in] argCity - dodawane miasto
 */
void addQueue(Queue **argQueue, City *argCity);

/** @brief Funkcja zwraca poczatek kolejki i go usuwa.
 * Funkcja zwraca poczatek kolejki i go usuwa
 * jesli kolejka jest pusta to zwraca null.
 * @param[in,out] argQueue - edytowana kolejka
 * @return Wskazni na miasto z poczatku kolejki
 */
City *frontQueue(Queue **argQueue);

/** @brief Funkcja sprawdzajaca czy kolejka jest pusta.
 * @param[in] argQueue - sprawdzana kolejka
 * @return @p true, jesli kolejka jest pusta, @p false wpp
 */
bool isEmptyQueue(Queue *argQueue);

/** @brief Funkcja usuwajaca z pamieci cala strukture.
 * Funkcja usuwa z pamieci cala strukture i jej skladowe.
 * @param[in,out] argQueue - usuwana kolejka
 */
void deleteQueue(Queue **argQueue);

/** @brief Funckja przechodzaca po sasiednich miastach.
 * Funckja przechodzi po sasiednich miatach,
 * dodaje do kolejki te, ktore spelniaja zlozenia SPFA.
 * @param[in] argCity - dane miasto
 * @param[in] argCityMeta - miasto koncowe w SPFA
 * @param[in,out] argQ - edytowana kolejka
 * @param[in] routeId - edytowana droga krajowa
 */
void allNeighbours(City *argCity, City *argCityMeta, Queue *argQ, unsigned routeId);

/** @brief Funkcja zmieniajaca wartosc czy miasto jest w kolejce.
 * @param[in,out] argCity - dane misto
 * @param[in] argB - ustawiana wartosc logiczna
 */
void setCityQ(City *argCity, bool argB);

/** @brief Funkcja dodajaca miasto to drogi krajowej.
 * @param[in,out] argCity - edytowane miasto
 * @param[in] routeId - id drogi
 */
void setRouteCity(City *argCity, unsigned routeId);

/** @brief Funkcja usuwa krawedz miedzy miastami.
 * @param[in,out] argCity1 - miasto nr 1
 * @param[in,out] argCity2 - miasto nr 2
 */
void removeRoadCity(City *argCity1, City *argCity2);

/** @brief Funkcja sprawdza czy miasto jest w danej drodze krajowej.
 * @param[in] argCity - dane miasto
 * @param[in] routeId - id drogi krajowej
 * @return @p true, jesli miasto jest w tej drodze krajowej, @p false wpp
 */
bool isInRouteCity(City *argCity, unsigned routeId);

/** @brief Funkcja zwraca nastepne miasto nalezace do drogi krajowej.
 * Funkcja zwraca sasiednie, nieodwiedzone miasto,
 * ktore nalezy do drogi krajowej
 * @param[in] argCity - dane miasto
 * @param[in] argPrevCity - poprzednie miasto
 * @param[in] routeId - id drogi krajowej
 * @return Wskaznik na sasiednie miasto
 */
City *getNextInRoute(City *argCity, City *argPrevCity, unsigned routeId);

/** @brief Funkcja zwraca rok dla danego miasta, zgodny z SPFA.
 * @param[in] argRoad - dane miasto
 * @return Rok zgodny z SPFA.
 */
int getRoadYear(Road *argRoad);

/** @brief Funkcja zwraca dlugosc drogi.
 * @param[in] argRoad - dana droga
 * @return Dlugosc danej drogi, @p 0 jesli zly parametr
 */
unsigned getRoadLength(Road *argRoad);

/** @brief Funcja ustawiajaca dla drogi nalezenie do drogi krajowej.
 * @param[in,out] argRoad - edytowana droga
 * @param[in] routeId - dana droga krajowa
 */
void setRoadRoute(Road *argRoad, unsigned routeId);

/** @brief Funkcja sprawdzajaca czy droga nalezy do drogi krajowej.
 * @param[in] argRoad - dana droga
 * @param[in] routeId - id drogi krajowej
 * @return @p true jesli nalezy do drogi krajowej, @p false wpp
 */
bool getRoadRoute(Road *argRoad, unsigned routeId);

/** @brief Funkcja usuwa droge z drogi krajowej.
 * Funkcja usuwa droge z drogi krajowej, jesli
 * droga krajowa nie istenie, droga nie istnieje,
 * to zwraca @p false.
 *
 * @param[in,out] argRoad - edytowana droga
 * @param[in] routeId - id usuwanej drogi krajowej
 */
void removeRoadRoute(Road *argRoad, unsigned routeId);

/** @brief Funkcjja usuwa miasto z drogi krajowej.
 * Funkcja usuwa miasto z drogi krajowej, jesli miasto
 * nie istnieje, droga krajowa nie istnieje to
 * zwraca @p false.
 *
 * @param[in,out] argCity - edytowane miasto
 * @param[in] routeId - id usuwanej drogi krajowej
 */
void removeCityRoute(City *argCity, unsigned routeId);

#endif //DROGI_CITIESANDROADS_H
