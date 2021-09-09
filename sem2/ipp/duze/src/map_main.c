/** @file
 * Implementacja interface'u tekstowego.
 */

#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>
#include <inttypes.h>
#include "map.h"
#include "string.h"
#include "parser.h"

/** @brief Funckja usuwajaca z pamieci tablice.
 * @param[in,out] argTab - usuwana tablica
 * @param[in] tabSize - rozmiar tablicy
 */
void cleanTab(char **argTab, unsigned tabSize) {
    for (unsigned i = 0; i < tabSize; i++) {
        if (*(argTab + i)) {
            free(*(argTab + i));
        }
    }

    free(argTab);
} //cleanTab

/** @brief Funkcja tworzy droge krajowa na podstawie opisu.
 * Funckja tworzy droge krajowa w nastepujacej kolejnosci: \n
 *
 * Sprawdza czy numer drogi miesci sie w zakresie [1; 999] (poprawnosc argumentu),
 * jesli nie zwraca @p false. (brak zmian)\n
 *
 * Sprawdza czy istnieje droga o danym id, jesli istnieje zwraca @p false. (brak zmian)\n
 *
 * Sprawdza czy mozliwe jest dodanie wszystkich drog, tj
 * czy nie ma samoprzeciec, jesli istnieje droga miedzy danymi miastami,
 * to czy dlugosc jest dobra i da sie poprawic rok, czy argumenty sa poprawne. (brak zmian)\n
 *
 * Tworzy brakujace drogi, poprawia rok remontu/budowy,
 * jesli wystapi jakis blad to zwraca @p false. (pozostawia zmiany) \n
 *
 * Tworzy droge krajowa i kazdej drodze przypisuje przynaleznosc do drogi krajowej, jesli operacja sie nie powiedzie
 * zwraca @p false. (pozostawia zmiany) \n \n
 *
 * Jesli wszystkie opercaje sie powiodly, skladnia calego polecenia jest poprawna,
 * wszystkie argumenty sa poprawne, alokowanie pamieci sie powiodlo, to funkcja zwraca @p true.
 *
 *
 * @param[in,out] map - wskaznik na strukture przechowujaca mape drog;
 * @return @p true, jesli tworzenie drogi sie powiodlo, @p false wpp
 */
bool makeRoute(Map *map) {
    int64_t routeId = getNumber();

    if (routeId <= 0 || routeId > 999 || isSemicolon() == false) {
        return false;
    }

    unsigned tabIndex = 0;
    unsigned tabSize = 1;
    char **cityNamesTab = malloc(sizeof(char*) * tabSize);
    if (cityNamesTab == NULL) {
        return false;
    }

    unsigned *lengthTab = malloc(sizeof(unsigned) * tabSize);
    if (lengthTab == NULL) {
        cleanTab(cityNamesTab, tabSize);

        return false;
    }

    int *yearTab = malloc(sizeof(int) * tabSize);
    if (yearTab == NULL) {
        cleanTab(cityNamesTab, tabSize);
        free(lengthTab);

        return false;
    }

    *cityNamesTab = getString();
    tabIndex++;

    if (*cityNamesTab == NULL || checkString(*cityNamesTab) == false) {
        cleanTab(cityNamesTab, tabIndex);
        free(lengthTab);
        free(yearTab);

        return false;
    }

    while (isEOF() == false && isNewLine() == false) {
        if (isSemicolon() == false) {
            cleanTab(cityNamesTab, tabIndex);
            free(lengthTab);
            free(yearTab);

            return false;
        }

        if (tabIndex == tabSize) {
            tabSize *= 2;
            cityNamesTab = realloc(cityNamesTab, sizeof(char*) * tabSize);
            if (cityNamesTab == NULL) {
                free(lengthTab);
                free(yearTab);

                return false;
            }

            lengthTab = realloc(lengthTab, sizeof(unsigned) * tabSize);
            if (lengthTab == NULL) {
                cleanTab(cityNamesTab, tabIndex);
                free(yearTab);

                return false;
            }

            yearTab = realloc(yearTab, sizeof(int) * tabSize);
            if (yearTab == NULL) {
                cleanTab(cityNamesTab, tabIndex);
                free(lengthTab);

                return false;
            }
        }

        int64_t length = getNumber();
        if (length <= 0 || isSemicolon() == false || length > UINT32_MAX) {
            cleanTab(cityNamesTab, tabIndex);
            free(lengthTab);
            free(yearTab);

            return false;
        }
        *(lengthTab + tabIndex) = (unsigned)length;

        int64_t year = getNumber();
        if (year == 0 || isSemicolon() == false || year > INT32_MAX || year < INT32_MIN) {
            cleanTab(cityNamesTab, tabIndex);
            free(lengthTab);
            free(yearTab);

            return false;
        }
        *(yearTab + tabIndex) = (int)year;

        if (isNewLine()) {
            cleanTab(cityNamesTab, tabIndex);
            free(lengthTab);
            free(yearTab);

            return false;
        }

        *(cityNamesTab + tabIndex) = getString();
        if (*(cityNamesTab + tabIndex) == NULL || checkString(*(cityNamesTab + tabIndex)) == false) {
            cleanTab(cityNamesTab, tabIndex + 1);
            free(lengthTab);
            free(yearTab);

            return false;
        }


        for (unsigned i = 0; i < tabIndex; i++) {
            if (compareString(*(cityNamesTab + i), *(cityNamesTab + tabIndex))) {
                cleanTab(cityNamesTab, tabIndex + 1);
                free(lengthTab);
                free(yearTab);

                return false;
            }
        }

        if (canAddRoad(map, *(cityNamesTab + tabIndex - 1), *(cityNamesTab + tabIndex),
                (unsigned)length, (int)year) == false) {
            cleanTab(cityNamesTab, tabIndex + 1);
            free(lengthTab);
            free(yearTab);

            return false;
        }

        tabIndex++;
    }

    if (isNewLine() == false) {
        cleanTab(cityNamesTab, tabIndex);
        free(lengthTab);
        free(yearTab);

        return false;
    }

    if (tabIndex == 1 || findRouteMap(map, (unsigned)routeId)) {
        cleanTab(cityNamesTab, tabIndex);
        free(lengthTab);
        free(yearTab);

        return false;
    }

    for (unsigned i = 1; i < tabIndex; i++) {
        if (compareRoadLength(map, *(cityNamesTab + i - 1), *(cityNamesTab + i), *(lengthTab + i))) {
            if (repairRoad(map, *(cityNamesTab + i - 1), *(cityNamesTab + i), *(yearTab + i)) == false) {
                cleanTab(cityNamesTab, tabIndex);
                free(lengthTab);
                free(yearTab);

                return false;
            }
        } else if (addRoad(map, *(cityNamesTab + i - 1), *(cityNamesTab + i),
                           *(lengthTab + i), *(yearTab + i)) == false) {
            cleanTab(cityNamesTab, tabIndex);
            free(lengthTab);
            free(yearTab);

            return false;
        }
    }

    if (newRouteWithParam(map, *cityNamesTab, *(cityNamesTab + tabIndex - 1), (unsigned)routeId) == false) {
        cleanTab(cityNamesTab, tabIndex);
        free(lengthTab);
        free(yearTab);

        return false;
    }

    for (unsigned i = 1; i < tabIndex; i++) {
        if (setRouteForRoad(map, *(cityNamesTab + i - 1), *(cityNamesTab + i), (unsigned)routeId) == false) {
            cleanTab(cityNamesTab, tabIndex);
            free(lengthTab);
            free(yearTab);

            return false;
        }
    }

    cleanTab(cityNamesTab, tabIndex);
    free(lengthTab);
    free(yearTab);

    return true;
} //makeRoute


/** @brief Wywoluje funkcja addRoad z map.h
 * Funkcja sprawdza dane wejsciowe i wywoluje
 * addRoad z map.h, zwraca to co wywolywana funkcja.
 * @param[in] map -  wskaznik na strukture przechowujaca mape drog;
 * @return @p false, jesli dane sa niepoprawde, wpp to co @see addRoad
 */
bool triggerAddRoad(Map *map) {
    if (isSemicolon() == false) {
        return false;
    }

    char *cityName1 = getString();
    if (isSemicolon() == false) {
        free(cityName1);

        return false;
    }

    char *cityName2 = getString();
    if (isSemicolon() == false) {
        free(cityName1);
        free(cityName2);

        return false;
    }

    int64_t length = getNumber();
    if (length <= 0 || length > UINT32_MAX || isSemicolon() == false) {
        free(cityName1);
        free(cityName2);

        return false;
    }
    int64_t year = getNumber();
    if(year == 0 || year < INT32_MIN || year > INT32_MAX) {
        free(cityName1);
        free(cityName2);

        return false;
    }

    if (isNewLine() == false) {
        free(cityName1);
        free(cityName2);

        return false;
    }

    bool result = addRoad(map, cityName1, cityName2, (unsigned)length, (int)year);
    free(cityName1);
    free(cityName2);

    return result;
} //triggerAddRoad


/** @brief Wywoluje funkcja repairRoad z map.h
 * Funkcja sprawdza dane wejsciowe i wywoluje
 * repairRoad z map.h, zwraca to co wywolywana funkcja.
 * @param[in] map -  wskaznik na strukture przechowujaca mape drog;
 * @return @p false, jesli dane sa niepoprawde, wpp to co @see repairRoad
 */
bool triggerRepairRoad(Map *map) {
    if (isSemicolon() == false) {
        return false;
    }

    char *cityName1 = getString();
    if (cityName1 == NULL || isSemicolon() == false) {
        free(cityName1);

        return false;
    }

    char *cityName2 = getString();
    if (cityName2 == NULL || isSemicolon() == false) {
        free(cityName1);
        free(cityName2);

        return false;
    }

    int64_t year = getNumber();
    if (year == 0 || year > INT32_MAX || year < INT32_MIN || isNewLine() == false) {
        free(cityName1);
        free(cityName2);

        return false;
    }

    bool result = repairRoad(map, cityName1, cityName2, (int)year);

    free(cityName1);
    free(cityName2);

    return result;
} //triggerRepairRoad


/** @brief Wywoluje funkcja getRouteDescription z map.h
 * Funkcja sprawdza dane wejsciowe i wywoluje
 * getRouteDescription z map.h, zwraca to co wywolywana funkcja.
 * @param[in] map -  wskaznik na strukture przechowujaca mape drog;
 * @return @p false, jesli dane sa niepoprawde, wpp to co @see getRouteDescription
 */
bool triggerGetRouteDescription(Map *map) {
    if (isSemicolon() == false || isDigitNext() == false) {
        return false;
    }

    int64_t routeId = getNumber();
    if (routeId < 0 || routeId > UINT32_MAX) {
        return false;
    }

    if (isNewLine() == false) {
        return false;
    }

    const char *resString = getRouteDescription(map, (unsigned)routeId);
    if (resString == NULL) {
        return false;
    }

    fprintf(stdout, "%s\n", resString);

    free((void*)resString);

    return true;
} //triggerGetRouteDescription


/** @brief Funckja wywoluje NewRoute z map.h.
 * Funkcja wczytuje ze wejsica id drogi, nazwy miast i
 * tworzy nowa droge krajowa, jesli operacja wczytania sie nie
 * powiedzie, albo format wejscia jest niepoprawny to
 * zwraca @p false.
 * Jesli wszystko sie powiedzie zwraca @p true.
 *
 * @param[in,out] map - wskaznik na strukture przechowujaca mape drog;
 * @return @p false jesli dane sa nieporawne, wpp to co @see NewRoute;
 */
bool triggerNewRoute(Map *map) {
    if (isSemicolon() == false || isDigitNext() == false) {
        return false;
    }

    int64_t routeId = getNumber();
    if (routeId < 0 || routeId > UINT32_MAX || isSemicolon() == false) {
        return false;
    }

    char *cityName1 = getString();
    if (cityName1 == NULL || isSemicolon() == false) {
        free(cityName1);

        return false;
    }

    char *cityName2 = getString();
    if (cityName2 == NULL || isNewLine() == false) {
        free(cityName1);
        free(cityName2);

        return false;
    }

    bool res = newRoute(map, (unsigned)routeId, cityName1, cityName2);

    free(cityName1);
    free(cityName2);

    return res;
}


/** @brief Funkcja wywoluje extendRoute z map.h.
 * Funcje wczytuje z wejscia id drogi i nazwe miasta,
 * jesli operacja wczytania sie nie powiedzie, lub
 * format danych jest nieporawny to zwraca @p false.
 * Jesli wszystko sie powiedzie to zwraca @p true.
 *
 * @param[in,out] map - wskaznik na strukture przechowujaca mape drog;
 * @return @p false jesli dane sa niepoprawne, wpp to co @see extendRoute;
 */
bool triggerExtendRoute(Map *map) {
    if (isSemicolon() == false || isDigitNext() == false) {
        return false;
    }

    int64_t routeId = getNumber();
    if (routeId < 0 || routeId > UINT32_MAX || isSemicolon() == false) {
        return false;
    }

    char *cityName1 = getString();
    if (cityName1 == NULL || isNewLine() == false) {
        free(cityName1);

        return false;
    }

    bool res = extendRoute(map, (unsigned)routeId, cityName1);

    free(cityName1);

    return res;
}


/** @brief Funkcja wywoluje removeRoad z map.h.
 * Funkcja wczytuje dane z wejscia, jesli operacja
 * wczytywania sie nie powiedzie lub format danych
 * jest niepoprawny zwraca @p false.
 * Jesli wszystko sie powiedzie zwraca @p true.
 *
 * @param[in,out] map - wskaznik na strukture przechowujaca mape drog;
 * @return @p false jesli dane sa niepoprawne, wpp to co @see removeRoad;
 */
bool triggerRemoveRoad(Map *map) {
    if (isSemicolon() == false) {
        return false;
    }

    char *cityName1 = getString();
    if (cityName1 == NULL || isSemicolon() == false) {
        free(cityName1);

        return false;
    }

    char *cityName2 = getString();
    if (cityName2 == NULL || isNewLine() == false) {
        free(cityName1);
        free(cityName2);

        return false;
    }

    bool res = removeRoad(map, cityName1, cityName2);

    free(cityName1);
    free(cityName2);

    return res;
} //triggerRemoveRoad


/** @brief Funkcja wywoluje removeRoute z map.h.
 * Funkcja wczytuje dane i wywoluje removeRoute,
 * jesli dane sa niepoprawne, badz format danych
 * jest niepoprawny to zwraca @p false.
 * Jesli wszystko sie powiedzie zwraca @p true.
 *
 * @param[in,out] map - wskaznik na strukture przechowujaca mape drog;
 * @return @p false jesli dane sa niepoprawne, wpp to co @see removeRoad;
 */
bool triggerRemoveRoute(Map *map) {
    if (isSemicolon() == false || isDigitNext() == false) {
        return false;
    }

    int64_t routeId = getNumber();
    if (routeId < 0 || routeId > UINT32_MAX || isNewLine() == false) {
        return false;
    }

    return removeRoute(map, (unsigned)routeId);
} //triggerRemoveRoute


/** @brief Funkcja main, przetwarza operacje wejscia.
 * @return Kody wyjscia programu
 */
int main() {
    Map *map = newMap();

    for (int typeOfOperation = getTypeOfOperation();
        typeOfOperation != 5; typeOfOperation = getTypeOfOperation()) {

        if (typeOfOperation == 0) {
            raiseError();
        }
        else if (typeOfOperation == 1) {
            if (makeRoute(map) == false) {
                raiseError();
            }
        }
        else if (typeOfOperation == 2) {
            if (triggerAddRoad(map) == false) {
                raiseError();
            }
        }
        else if (typeOfOperation == 3) {
            if (triggerRepairRoad(map) == false) {
                raiseError();
            }
        }
        else if (typeOfOperation == 4) {
            if (triggerGetRouteDescription(map) == false) {
                raiseError();
            }
        }
        else if (typeOfOperation == 7) {
            if (triggerNewRoute(map) == false) {
                raiseError();
            }
        }
        else if (typeOfOperation == 8) {
            if (triggerExtendRoute(map) == false) {
                raiseError();
            }
        }
        else if (typeOfOperation == 9) {
            if (triggerRemoveRoad(map) == false) {
                raiseError();
            }
        }
        else if (typeOfOperation == 10) {
            if (triggerRemoveRoute(map) == false) {
                raiseError();
            }
        }
        skipOneLine();
    }

    deleteMap(map);

    return 0;
} //main
