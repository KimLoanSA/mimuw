/** @file
 * Interfejs klasy przechowującej mapę dróg krajowych
 *
 * @author Łukasz Kamiński <kamis@mimuw.edu.pl>, Marcin Peczarski <marpe@mimuw.edu.pl>
 * @copyright Uniwersytet Warszawski
 * @date 20.03.2019
 */


#ifndef __MAP_H__
#define __MAP_H__

#include <stdbool.h>

/**
 * Struktura przechowująca mapę dróg krajowych.
 */
typedef struct Map Map;

/** @brief Tworzy nową strukturę.
 * Tworzy nową, pustą strukturę niezawierającą żadnych miast, odcinków dróg ani
 * dróg krajowych.
 * @return Wskaźnik na utworzoną strukturę lub NULL, gdy nie udało się
 * zaalokować pamięci.
 */
Map* newMap(void);

/** @brief Usuwa strukturę.
 * Usuwa strukturę wskazywaną przez @p map.
 * Nic nie robi, jeśli wskaźnik ten ma wartość NULL.
 * @param[in] map        – wskaźnik na usuwaną strukturę.
 */
void deleteMap(Map *map);

/** @brief Dodaje do mapy odcinek drogi między dwoma różnymi miastami.
 * Jeśli któreś z podanych miast nie istnieje, to dodaje go do mapy, a następnie
 * dodaje do mapy odcinek drogi między tymi miastami.
 * @param[in,out] map    – wskaźnik na strukturę przechowującą mapę dróg;
 * @param[in] city1      – wskaźnik na napis reprezentujący nazwę miasta;
 * @param[in] city2      – wskaźnik na napis reprezentujący nazwę miasta;
 * @param[in] length     – długość w km odcinka drogi;
 * @param[in] builtYear  – rok budowy odcinka drogi.
 * @return Wartość @p true, jeśli odcinek drogi został dodany.
 * Wartość @p false, jeśli wystąpił błąd: któryś z parametrów ma niepoprawną
 * wartość, obie podane nazwy miast są identyczne, odcinek drogi między tymi
 * miastami już istnieje lub nie udało się zaalokować pamięci.
 */
bool addRoad(Map *map, const char *city1, const char *city2,
             unsigned length, int builtYear);

/** @brief Modyfikuje rok ostatniego remontu odcinka drogi.
 * Dla odcinka drogi między dwoma miastami zmienia rok jego ostatniego remontu
 * lub ustawia ten rok, jeśli odcinek nie był jeszcze remontowany.
 * @param[in,out] map    – wskaźnik na strukturę przechowującą mapę dróg;
 * @param[in] city1      – wskaźnik na napis reprezentujący nazwę miasta;
 * @param[in] city2      – wskaźnik na napis reprezentujący nazwę miasta;
 * @param[in] repairYear – rok ostatniego remontu odcinka drogi.
 * @return Wartość @p true, jeśli modyfikacja się powiodła.
 * Wartość @p false, jeśli wystąpił błąd: któryś z parametrów ma niepoprawną
 * wartość, któreś z podanych miast nie istnieje, nie ma odcinka drogi między
 * podanymi miastami, podany rok jest wcześniejszy niż zapisany dla tego odcinka
 * drogi rok budowy lub ostatniego remontu.
 */
bool repairRoad(Map *map, const char *city1, const char *city2, int repairYear);

/** @brief Łączy dwa różne miasta drogą krajową.
 * Tworzy drogę krajową pomiędzy dwoma miastami i nadaje jej podany numer.
 * Wśród istniejących odcinków dróg wyszukuje najkrótszą drogę. Jeśli jest
 * więcej niż jeden sposób takiego wyboru, to dla każdego wariantu wyznacza
 * wśród wybranych w nim odcinków dróg ten, który był najdawniej wybudowany lub
 * remontowany i wybiera wariant z odcinkiem, który jest najmłodszy.
 * @param[in,out] map    – wskaźnik na strukturę przechowującą mapę dróg;
 * @param[in] routeId    – numer drogi krajowej;
 * @param[in] city1      – wskaźnik na napis reprezentujący nazwę miasta;
 * @param[in] city2      – wskaźnik na napis reprezentujący nazwę miasta.
 * @return Wartość @p true, jeśli droga krajowa została utworzona.
 * Wartość @p false, jeśli wystąpił błąd: któryś z parametrów ma niepoprawną
 * wartość, istnieje już droga krajowa o podanym numerze, któreś z podanych
 * miast nie istnieje, obie podane nazwy miast są identyczne, nie można
 * jednoznacznie wyznaczyć drogi krajowej między podanymi miastami lub nie udało
 * się zaalokować pamięci.
 */
bool newRoute(Map *map, unsigned routeId,
              const char *city1, const char *city2);

/** @brief Wydłuża drogę krajową do podanego miasta.
 * Dodaje do drogi krajowej nowe odcinki dróg do podanego miasta w taki sposób,
 * aby nowy fragment drogi krajowej był najkrótszy. Jeśli jest więcej niż jeden
 * sposób takiego wydłużenia, to dla każdego wariantu wyznacza wśród dodawanych
 * odcinków dróg ten, który był najdawniej wybudowany lub remontowany i wybiera
 * wariant z odcinkiem, który jest najmłodszy.
 * @param[in,out] map    – wskaźnik na strukturę przechowującą mapę dróg;
 * @param[in] routeId    – numer drogi krajowej;
 * @param[in] city       – wskaźnik na napis reprezentujący nazwę miasta.
 * @return Wartość @p true, jeśli droga krajowa została wydłużona.
 * Wartość @p false, jeśli wystąpił błąd: któryś z parametrów ma niepoprawną
 * nazwę, nie istnieje droga krajowa o podanym numerze, nie ma miasta o podanej
 * nazwie, przez podane miasto już przechodzi droga krajowa o podanym numerze,
 * podana droga krajowa kończy się w podanym mieście, nie można jednoznacznie
 * wyznaczyć nowego fragmentu drogi krajowej lub nie udało się zaalokować
 * pamięci.
 */
bool extendRoute(Map *map, unsigned routeId, const char *city);

/** @brief Usuwa odcinek drogi między dwoma różnymi miastami.
 * Usuwa odcinek drogi między dwoma miastami. Jeśli usunięcie tego odcinka drogi
 * powoduje przerwanie ciągu jakiejś drogi krajowej, to uzupełnia ją
 * istniejącymi odcinkami dróg w taki sposób, aby była najkrótsza. Jeśli jest
 * więcej niż jeden sposób takiego uzupełnienia, to dla każdego wariantu
 * wyznacza wśród dodawanych odcinków drogi ten, który był najdawniej wybudowany
 * lub remontowany i wybiera wariant z odcinkiem, który jest najmłodszy.
 * @param[in,out] map    – wskaźnik na strukturę przechowującą mapę dróg;
 * @param[in] city1      – wskaźnik na napis reprezentujący nazwę miasta;
 * @param[in] city2      – wskaźnik na napis reprezentujący nazwę miasta.
 * @return Wartość @p true, jeśli odcinek drogi został usunięty.
 * Wartość @p false, jeśli z powodu błędu nie można usunąć tego odcinka drogi:
 * któryś z parametrów ma niepoprawną wartość, nie ma któregoś z podanych miast,
 * nie istnieje droga między podanymi miastami, nie da się jednoznacznie
 * uzupełnić przerwanego ciągu drogi krajowej lub nie udało się zaalokować
 * pamięci.
 */
bool removeRoad(Map *map, const char *city1, const char *city2);

/** @brief Udostępnia informacje o drodze krajowej.
 * Zwraca wskaźnik na napis, który zawiera informacje o drodze krajowej. Alokuje
 * pamięć na ten napis. Zwraca pusty napis, jeśli nie istnieje droga krajowa
 * o podanym numerze. Zaalokowaną pamięć trzeba zwolnić za pomocą funkcji free.
 * Informacje wypisywane są w formacie:
 * numer drogi krajowej;nazwa miasta;długość odcinka drogi;rok budowy lub
 * ostatniego remontu;nazwa miasta;długość odcinka drogi;rok budowy lub
 * ostatniego remontu;nazwa miasta;…;nazwa miasta.
 * Kolejność miast na liście jest taka, aby miasta @p city1 i @p city2, podane
 * w wywołaniu funkcji @ref newRoute, które utworzyło tę drogę krajową, zostały
 * wypisane w tej kolejności.
 * @param[in,out] map    – wskaźnik na strukturę przechowującą mapę dróg;
 * @param[in] routeId    – numer drogi krajowej.
 * @return Wskaźnik na napis lub NULL, gdy nie udało się zaalokować pamięci.
 */
char const* getRouteDescription(Map *map, unsigned routeId);

/** @brief Funkcja ustawia dla danej drogi przynaleznosc do drogi krajowej.
 * Funkcja ustawia dla drogi miedzy danymi miastami przynaleznosc do drogi krajowej.
 * Jesli nie istnieje dana droga krajowa, badz droga miedzy danymi miastami to zwraca @p false.
 * @param[in,out] map - wskaznik na strukture przechowujaca mape drog;
 * @param[in] city1 - wskaźnik na napis reprezentujący nazwę miasta;
 * @param[in] city2 - wskaźnik na napis reprezentujący nazwę miasta;
 * @param[in] routeId - numer drogi krajowej;
 * @return @p true jesli operacja sie powiodla, @p false jesli prametry sa niewlasciwe,
 * badz droga krajowa, badz droga nie istnieja.
 */
bool setRouteForRoad(Map *map, const char *city1, const char *city2, unsigned routeId);

/** @brief Funkcja porownujaca dlugosc drogi z dana wartoscia.
 * Funkcja porownuje droge miedzy danymi miastami z dana wartoscia,
 * jesli droga nie istnieje zwraca false
 * @param[in] map - wskaznik na strukture przechowujaca mape drog;
 * @param[in] city1 - wskaźnik na napis reprezentujący nazwę miasta;
 * @param[in] city2 - wskaźnik na napis reprezentujący nazwę miasta;
 * @param[in] length - dlugosc do porownania
 * @return @p true jesli dlugosc drogi jest rowna danej warosci, @p false wpp
 */
bool compareRoadLength(Map *map, const char *city1, const char *city2, unsigned length);

/** @brief Funkcja sprawdza czy mozna dodac droge o danej dlugosci i roku.
 * Funkcja sprawdza czy mozna dodac do mapy dorge miedzy danymi miastami.
 * Jesli droga istenije, to sprawdza jej dlugosc, jesli jest taka sama
 * to sprawdza czy da sie naprawic rok.
 * Jesli dllugosc jest inna to sie nie da dodac.
 * @param[in] map - wskaznik na strukture przechowujaca mape drog;
 * @param[in] city1 - wskaźnik na napis reprezentujący nazwę miasta;
 * @param[in] city2 - wskaźnik na napis reprezentujący nazwę miasta;
 * @param[in] length - dlugosc do porownania;
 * @param[in] year - rok do porownania;
 * @return @p true jesli mozna dodac droge o danych parametrach, @p false wpp
 */
bool canAddRoad(Map *map, const char *city1, const char *city2, unsigned length, int year);

/** @brief Funkcja dodaje nowa droge krajowa zaczynajaca sie w danych miastach.
 * Funkcja tworzy nowa droge krajowa z danymi koncami, ale nie odpala SPFA.
 * Zwraca false jesli jakas struktura nie istenieje, badz istnieje juz droga
 * krajowa o danym id.
 * @param[in, out] map - wskaznik na strukture przechowujaca mape drog;
 * @param[in] city1 - wskaźnik na napis reprezentujący nazwę miasta;
 * @param[in] city2 - wskaźnik na napis reprezentujący nazwę miasta;
 * @param[in] routeId - numer drogi krajowej;
 * @return @p true jesli operacja sie powiodla, @p false wpp
 */
bool newRouteWithParam(Map *map, const char *city1, const char *city2, unsigned routeId);

/** @brief Funkcja sprawdza czy istnieje droga krajowa o danym numerze.
 * @param[in] map - wskaznik na strukture przechowujaca mape drog;
 * @param[in] routeId - id szukanej drogi krajowej;
 * @return @p true jesli istnieje droga o danym id, @p false wpp
 */
bool findRouteMap(Map *map, unsigned routeId);

/** @brief Funckja usuwa z mapy droge krajowa o danym id.
 * Funkcja usuwa z mapy droge krajowa o danym id,
 * jesli nie istnieje taka droga, jesli dane
 * sa niepoprawne, badz nie istnieje droga
 * zwraca @p false.
 *
 * @param[in,out] map - wskaznik na strukture przechowujaca mape drog;
 * @param[in] routeId - numer drogi krajowej;
 * @return @p false jesli wystapil blad, @p true jesli udalo sie usunac droge krajowa
 */
bool removeRoute(Map *map, unsigned routeId);

#endif /* __MAP_H__ */
