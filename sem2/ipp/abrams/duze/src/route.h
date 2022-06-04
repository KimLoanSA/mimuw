/** @file
 * Interfaca klasy reprezentujacej drogi krajowe.
 */

#ifndef DROGI_ROUTE_H
#define DROGI_ROUTE_H

/**
 * Struktura przechowujaca drogi krajowe.
 */
typedef struct Route Route;

/** @brief Funkcja tworzaca nowa strukture drog krajowych.
 * Funkcja alokuja pamiec i tworzy nowa strukture,
 * jesli sie nie uda to zwraca NULL.
 * @return Wskaznik na nowa strukture, NULL jesli sie nie udalo
 */
Route *createRoute();

/** @brief Funckja dodajaca do struktury nowa droge krajowa.
 * Funkcja dodaje do struktury nowa droge krajowa
 * o danym id i daje jej konce.
 * @param[in,out] argRoute - edytowana struktura
 * @param[in] argId - id dodawanej drogi
 * @param[in] argCity1 - miasto poczatkowe
 * @param[in] argCity2 - miasto koncowe
 */
void addRoute(Route *argRoute, unsigned argId, City *argCity1, City *argCity2);

/** @brief Funkcja znjadujaca droge.
 * Funckja znajduje dana droge krajowa,
 * jesli id jest wieksze niz 999 lub
 * nie istnieje taka droga zwraca false.
 * @param[in] argRoute - przeszukiwana struktura
 * @param[in] argId - id szukanej drogi
 * @return @p true, jesli istnieje taka droga, @p false wpp
 */
bool findRoute(Route *argRoute, unsigned argId);

/** @brief Funkcja zwracajaca pierwsze z miast bedacymi koncami drogi.
 * Funckja zwraca poczatek drogi krajowej,
 * jesli sie nie powiedzie zwraca NULL.
 * @param[in] argRoute - przeszukiwana struktura
 * @param[in] argId - id drogi
 * @return Wskaznik na iasto bedace poczatkiem drogi
 */
City *getCity1Route(Route *argRoute, unsigned argId);

/** @brief Funkcja zwracajaca drugie z miast bedacymi koncami drogi.
 * Funckja zwraca koniec drogi krajowej,
 * jesli sie nie powiedzie zwraca NULL.
 * @param[in] argRoute - przeszukiwana struktura
 * @param[in] argId - id drogi
 * @return Wskaznik na iasto bedace koncem drogi
 */
City *getCity2Route(Route *argRoute, unsigned argId);

/** @brief Funkcja edytujaca koniec drogi krajowej.
 * Funcja edytuje koniec drogi krajowej,
 * zamiania podany koniec na nowy.
 * @param[in,out] argRoute - edytowana strukura
 * @param[in] argId - id drogi
 * @param[in] argOldCity - stare, zamianiane miasto
 * @param[in] argNewCity - nowe miasto
 */
void editCityRoute(Route *argRoute, unsigned argId, City *argOldCity, City *argNewCity);

/** @brief Funckja usuwa jedna droge krajowa.
 * Funckjaj usuwa ze struktury droge krajowa,
 * struktura nie istnieje lub droga nie istnieje
 * nic nie robi.
 * @param[in, out] argRoute - edytowana struktura
 * @param[in] argId - id usuwanej drogi krajowej
 */
void removeSingleRoute(struct Route *argRoute, unsigned argId);

/** @brief Funkcja usuwajaca sturukture.
 * Funkcja usuwa z pamieci cala strukture i jej skladowe.
 * @param[in,out] argRoute - usuwana struktura
 */
void deleteRoute(Route **argRoute);

#endif //DROGI_ROUTE_H
