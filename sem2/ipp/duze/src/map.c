/** @file
 * Implementacja klasy przechowującej mapę dróg krajowych
 */

#include <stdio.h>
#include <inttypes.h>
#include <stdlib.h>
#include <stdbool.h>
#include "bitset.h"
#include "randomIdGenerator.h"
#include "citiesAndRoads.h"
#include "hashmap.h"
#include "route.h"
#include "map.h"
#include "string.h"

/**
 * Struktura przechowująca mapę dróg krajowych.
 */
struct Map {
    Hashmap *hashmap; ///< haszmapa miast
    RandomIdGen *randomIdGen; ///< generator id
    Route *route; ///< drogi krajowe
};



/** @brief Funkcja aktualizujaca najkrotsza sciezke.
 * Funkcja przechodzi po najkrotszej sciezce
 * miedzy dwoma miastami i aktualizuje informacje
 * w miastach na niej.
 * @param[in] argCityStart - miasto startowe
 * @param[in] argCityMeta - miasto koncowe
 * @param[in] routeId - id aktualizowanej drogi krajowej
 */
void visitCities(City *argCityStart, City *argCityMeta, unsigned routeId) {
    City *tempCity = argCityMeta;

    while (tempCity != argCityStart) {
        setRouteCity(tempCity, routeId);

        Road *tempRoad = findRoad(tempCity, getCityPrev(tempCity));
        setRoadRoute(tempRoad, routeId);

        tempCity = getCityPrev(tempCity);
    }

    setRouteCity(argCityStart, routeId);
} //visitCities

/** @brief Algorytm znajdowania najkrotszej sciezki miedzy miastami.
 * Funkcja znajduje najkrotsza sciezke miedzy danymi miastami
 * uzywajac algorytmu spfa. Aktualizuje inforamacje miast na sciezce,
 * jesli @p visit jest @p true. Zwraca @p 1 jesli dorga jest jednoznacza,
 * @p 0 jesli niejednoznaczna, @p 2 jesli sie nie istnieje sciezka miedzy miastami.
 * @param[in,out] map - edytowana mapa
 * @param[in] argCityStart - miasto startowe
 * @param[in] argCityMeta - miasto koncowe
 * @param[in] routeId - aktualizowana droga krajowa
 * @param[in] visit - parametr, okreslajacy czy edytowac info na sciezce
 * @return @p 0 sciezka jest niejednoznaczna, @p 1 sciezka jest jednoznaczna, @p 2 nie istnieje sciezka
 */
int spfa(Map *map, City *argCityStart, City *argCityMeta, unsigned routeId, bool visit) {
    resetAllHashmap(map->hashmap);

    setCityDistance(argCityStart, 0);
    setCityQ(argCityStart, true);

    Queue *q = createQueue();
    if (q == NULL) {
        return 0;
    }

    addQueue(&q, argCityStart);

    while (isEmptyQueue(q) == false) {
        City *actCity = frontQueue(&q);
        setCityQ(actCity, false);

        allNeighbours(actCity, argCityMeta, q, routeId);
    }

    deleteQueue(&q);

    if (isGoodCity(argCityMeta) == false) {
        return 0;
    }

    if (getCityDistance(argCityMeta) == UINT64_MAX) {
        return 2;
    }

    if (visit) {
        visitCities(argCityStart, argCityMeta, routeId);
    }

    return 1;
} //spfa


Map* newMap(void) {
    Map *resMap = malloc(sizeof(Map));
    if (resMap == NULL) {
        return NULL;
    }

    resMap->hashmap = createHashmap();
    if (resMap->hashmap == NULL) {
        return NULL;
    }

    resMap->randomIdGen = createIdGen();
    if (resMap->randomIdGen == NULL) {
        return NULL;
    }

    resMap->route = createRoute();
    if (resMap->route == NULL) {
        return NULL;
    }

    return resMap;
} //newMap


void deleteMap(Map *map) {
    if (map == NULL) {
        return;
    }

    deleteHashmap(&(map->hashmap));
    deleteGen(&(map->randomIdGen));
    deleteRoute(&(map->route));

    free(map);
} //deleteMap


bool addRoad(Map *map, const char *city1, const char *city2,
             unsigned length, int builtYear) {
    if (map == NULL || city1 == NULL || city2 == NULL ||
        builtYear == 0 ||city1 == city2 || length == 0) {
        return false;
    }

    if (checkString(city1) == false || checkString(city2) == false) {
        return false;
    }

    if (compareString(city1, city2)) {
        return false;
    }

    City *tempCity1 = findHashmap(map->hashmap, city1);
    City *tempCity2 = findHashmap(map->hashmap, city2);

    if (tempCity1 == NULL) {
        tempCity1 = createCity(city1, map->randomIdGen);
        addHashmap(map->hashmap, tempCity1);
    }

    if (tempCity2 == NULL) {
        tempCity2 = createCity(city2, map->randomIdGen);
        addHashmap(map->hashmap, tempCity2);
    }

    if (findRoad(tempCity1, tempCity2) != NULL) {
        return false;
    }

    return addRoadCity(tempCity1, tempCity2, length, builtYear);
} //addRoad


bool repairRoad(Map *map, const char *city1, const char *city2, int repairYear) {
    if (map == NULL || city1 == NULL || city2 == NULL || repairYear == 0) {
        return false;
    }

    if (checkString(city1) == false || checkString(city2) == false) {
        return false;
    }

    City *tempCity1 = findHashmap(map->hashmap, city1);
    City *tempCity2 = findHashmap(map->hashmap, city2);

    if (tempCity1 == NULL || tempCity2 == NULL) {
        return false;
    }

    Road *tempRoad = findRoad(tempCity1, tempCity2);
    if (tempRoad == NULL) {
        return false;
    }

    return setRoadYear(tempRoad, repairYear);
} //repairRoad


bool newRoute(Map *map, unsigned routeId,
              const char *city1, const char *city2) {
    if (map == NULL || city1 == NULL || city2 == NULL ||
        compareString(city1, city2) || routeId == 0 ||
        routeId > 999) {
        return false;
    }

    if (checkString(city1) == false || checkString(city2) == false) {
        return false;
    }

    if (findRoute(map->route, routeId)) {
        return false;
    }

    City *tempCity1 = findHashmap(map->hashmap, city1);
    City *tempCity2 = findHashmap(map->hashmap, city2);

    if (tempCity1 == NULL || tempCity2 == NULL) {
        return false;
    }

    if (spfa(map, tempCity1, tempCity2, routeId, false) != 1) {
        return false;
    }

    if (spfa(map, tempCity2, tempCity1, routeId, false) != 1) {
        return false;
    }

    spfa(map, tempCity1, tempCity2, routeId, true);
    addRoute(map->route, routeId, tempCity1, tempCity2);

    return true;
} //newRoute


bool extendRoute(Map *map, unsigned routeId, const char *city) {
    if (map == NULL || city == NULL || routeId == 0 || routeId > 999) {
        return false;
    }

    if (checkString(city) == false) {
        return false;
    }

    if (findRoute(map->route, routeId) == false) {
        return false;
    }

    City *tempCityStart = findHashmap(map->hashmap, city);
    if (tempCityStart == NULL) {
        return false;
    }

    if (isInRouteCity(tempCityStart, routeId)) {
        return false;
    }

    int spfa11 = spfa(map, getCity1Route(map->route, routeId), tempCityStart, routeId, false);
    int spfa12 = spfa(map, tempCityStart, getCity1Route(map->route, routeId), routeId, false);

    uint64_t dist1 = getCityDistance(getCity1Route(map->route, routeId));
    int year1 = getCityYear(getCity1Route(map->route, routeId));

    int spfa21 = spfa(map, getCity2Route(map->route, routeId), tempCityStart, routeId, false);
    int spfa22 = spfa(map, tempCityStart, getCity2Route(map->route, routeId), routeId, false);

    uint64_t dist2 = getCityDistance(getCity2Route(map->route, routeId));
    int year2 = getCityYear(getCity2Route(map->route, routeId));

    if (spfa11 == 2 && spfa21 == 2) {
        return false;
    }

    if (dist1 == dist2) {
        if  (year1 == year2) {
            return false;
        }

        if (year1 > year2) {
            if (spfa11 == 0 || spfa12 == 0) {
                return false;
            }

            spfa(map, tempCityStart, getCity1Route(map->route, routeId), routeId, true);
            editCityRoute(map->route, routeId, getCity1Route(map->route, routeId), tempCityStart);
        }
        else  {
            if (spfa21 == 0 || spfa22 == 0) {
                return false;
            }

            spfa(map, tempCityStart, getCity2Route(map->route, routeId), routeId, true);
            editCityRoute(map->route, routeId, getCity2Route(map->route, routeId), tempCityStart);
        }
    }
    else if (dist1 < dist2) {
        if (spfa11 == 0 || spfa12 == 0) {
            return false;
        }

        spfa(map, tempCityStart, getCity1Route(map->route, routeId), routeId, true);
        editCityRoute(map->route, routeId, getCity1Route(map->route, routeId), tempCityStart);
    }
    else {
        if (spfa21 == 0 || spfa22 == 0) {
            return false;
        }

        spfa(map, tempCityStart, getCity2Route(map->route, routeId), routeId, true);
        editCityRoute(map->route, routeId, getCity2Route(map->route, routeId), tempCityStart);
    }

    return true;
} //extendRoute


bool removeRoad(Map *map, const char *city1, const char *city2) {
    if (map == NULL || city1 == NULL || city2 == NULL) {
        return false;
    }

    if (checkString(city1) == false || checkString(city2) == false) {
        return false;
    }

    City *tempCity1 = findHashmap(map->hashmap, city1);
    City *tempCity2 = findHashmap(map->hashmap, city2);

    if (tempCity1 == NULL || tempCity2 == NULL) {
        return false;
    }

    Road *tempRoad = findRoad(tempCity1, tempCity2);
    if (tempRoad == NULL) {
        return false;
    }

    removeRoadCity(tempCity1, tempCity2);

    bool isOk = true;
    for (unsigned i = 1; i < 1000; i++) {
        if (getRoadRoute(tempRoad, i)) {
            if (spfa(map, tempCity1, tempCity2, i, false) != 1) {
                isOk = false;
                break;
            }

            if (spfa(map, tempCity2, tempCity1, i, false) != 1) {
                isOk = false;
                break;
            }
        }
    }

    if (isOk) {
        for (unsigned i = 1; i < 1000; i++) {
            if (getRoadRoute(tempRoad, i)) {
                spfa(map, tempCity1, tempCity2, i, true);
            }
        }

        deleteRoad(&tempRoad);
    }
    else {
        addRoadCityElem(tempCity1, tempCity2, tempRoad);
        return false;
    }

    return true;
} //removeRoad


/** @brief Funkcja obliczajaca dlugosc stringa opisujacego droge krajowa,
 * @param[in] argCity1 - miasto startowe
 * @param[in] argCity2 - miasto koncowe
 * @param[in] routeId - id danej drogi krajowej
 * @return Dlugosc stringa opisujacego droge krajowa
 */
unsigned lengthDescription(City *argCity1, City *argCity2, unsigned routeId) {
    unsigned resLength = numberLength(routeId) + 1;

    City *actCity = argCity1;
    City *prevCity = NULL;

    while (actCity != argCity2) {
        City *nextCity = getNextInRoute(actCity, prevCity, routeId);
        Road *tempRoad = findRoad(actCity, nextCity);

        resLength += stringLength(getCityName(actCity)) + 1;

        int tempYear = getRoadYear(tempRoad);

        if (tempYear < 0) {
            tempYear = -tempYear;
            resLength++;
        }
        resLength += numberLength((unsigned)tempYear) + 1;
        resLength += numberLength(getRoadLength(tempRoad)) + 1;

        prevCity = actCity;
        actCity = nextCity;
    }

    resLength += stringLength(getCityName(argCity2));

    return resLength;
} //lengthDescription


char const *getRouteDescription(Map *map, unsigned routeId) {
    if (map == NULL || routeId == 0 || routeId > 999) {
        char *res = malloc(sizeof(char));
        if (res == NULL) {
            return NULL;
        }

        *res = '\0';
        return res;
    }

    if (findRoute(map->route, routeId)) {
        City *tempCity1 = getCity1Route(map->route, routeId);
        City *tempCity2 = getCity2Route(map->route, routeId);
        unsigned length = lengthDescription(tempCity1, tempCity2, routeId);

        char *resString = malloc(sizeof(char) * (length + 1));
        if (resString == NULL) {
            return NULL;
        }

        City *actCity = tempCity1;
        City *prevCity = NULL;
        unsigned i = 0;

        char *routeIdS = numberToString(routeId);
        if (routeIdS == NULL) {
            return NULL;
        }

        for (unsigned j = 0; *(routeIdS + j) != '\0'; j++) {
            *(resString + i) = *(routeIdS + j);
            i++;
        }
        free(routeIdS);

        *(resString + i) = ';';
        i++;

        while (actCity != tempCity2) {
            City *nextCity = getNextInRoute(actCity, prevCity, routeId);
            Road *tempRoad = findRoad(actCity, nextCity);

            const char *tempName = getCityName(actCity);
            int tempYear = getRoadYear(tempRoad);
            unsigned tempLength = getRoadLength(tempRoad);

            for (unsigned j = 0; *(tempName + j) != '\0'; j++) {
                *(resString + i) = *(tempName + j);
                i++;
            }
            *(resString + i) = ';';
            i++;

            char *tempLengthS = numberToString(tempLength);
            if (tempLengthS == NULL) {
                return NULL;
            }

            for (unsigned j = 0;*(tempLengthS + j) != '\0'; j++) {
                *(resString + i) = *(tempLengthS + j);
                i++;
            }
            free(tempLengthS);

            *(resString + i) = ';';
            i++;

            if (tempYear < 0) {
                tempYear = -tempYear;
                *(resString + i) = '-';
                i++;
            }


            char *tempYearS = numberToString(tempYear);
            if (tempYearS == NULL) {
                return NULL;
            }

            for (unsigned j = 0; *(tempYearS + j) != '\0'; j++) {
                *(resString + i) = *(tempYearS + j);
                i++;
            }
            free(tempYearS);

            *(resString + i) = ';';
            i++;

            prevCity = actCity;
            actCity = nextCity;
        }

        const char *tempName = getCityName(tempCity2);
        for (unsigned j = 0; *(tempName + j) != '\0'; j++) {
            *(resString + i) = *(tempName + j);
            i++;
        }
        *(resString + i) = '\0';

        return resString;
    }
    else {
        char *res = malloc(sizeof(char));
        if (res == NULL) {
            return NULL;
        }

        *res = '\0';
        return res;
    }
} //getRouteDescription


bool setRouteForRoad(Map *map, const char *city1, const char *city2, unsigned routeId) {
    if (map == NULL || city1 == NULL || city2 == NULL ||
        routeId == 0 || routeId > 999) {
        return false;
    }

    if (findRoute(map->route, routeId) == false) {
        return false;
    }

    if (checkString(city1) == false || checkString(city2) == false) {
        return false;
    }

    City *tempCity1 = findHashmap(map->hashmap, city1);
    City *tempCity2 = findHashmap(map->hashmap, city2);


    if (tempCity1 == NULL || tempCity2 == NULL) {
        return false;
    }

    Road *tempRoad = findRoad(tempCity1, tempCity2);
    if (tempRoad == NULL) {
        return false;
    }

    setRouteCity(tempCity1, routeId);
    setRouteCity(tempCity2, routeId);
    setRoadRoute(tempRoad, routeId);

    return true;
} //setRoadRoute


bool compareRoadLength(Map *map, const char *city1, const char *city2, unsigned length) {
    if (map == NULL || city1 == NULL || city2 == NULL || length == 0) {
        return false;
    }

    City *tempCity1 = findHashmap(map->hashmap, city1);
    City *tempCity2 = findHashmap(map->hashmap, city2);
    if (tempCity1 == NULL || tempCity2 == NULL) {
        return false;
    }

    Road *tempRoad = findRoad(tempCity1, tempCity2);
    if (tempRoad == NULL) {
        return false;
    }

    return getRoadLength(tempRoad) == length;
} //compareRoadLength


bool canAddRoad(Map *map, const char *city1, const char *city2, unsigned length, int year) {
    if (map == NULL || city1 == NULL || city2 == NULL || length == 0 || year == 0) {
        return false;
    }

    City *tempCity1 = findHashmap(map->hashmap, city1);
    City *tempCity2 = findHashmap(map->hashmap, city2);
    if (tempCity1 == NULL || tempCity2 == NULL) {
        return true;
    }

    Road *tempRoad = findRoad(tempCity1, tempCity2);
    if (tempRoad == NULL) {
        return true;
    }

    if (getRoadLength(tempRoad) != length) {
        return false;
    }

    return getRoadYear(tempRoad) <= year;
} // canAddRoad


bool newRouteWithParam(Map *map, const char *city1, const char *city2, unsigned routeId) {
    if (map == NULL || city1 == NULL || city2 == NULL || routeId == 0 || routeId > 999) {
        return false;
    }

    if (findRoute(map->route, routeId)) {
        return false;
    }

    City *tempCity1 = findHashmap(map->hashmap, city1);
    City *tempCity2 = findHashmap(map->hashmap, city2);
    if (tempCity1 == NULL || tempCity2 == NULL) {
        return false;
    }

    addRoute(map->route, routeId, tempCity1, tempCity2);

    return true;
} //newRouteWithParam


bool findRouteMap(Map *map, unsigned routeId) {
    if (map == NULL || routeId == 0 || routeId > 999) {
        return false;
    }

    return findRoute(map->route, routeId);
}


bool removeRoute(Map *map, unsigned routeId) {
    if (map == NULL || routeId > 999 || routeId <= 0) {
        return false;
    }

    if (findRoute(map->route, routeId) == false) {
        return false;
    }

    City *tempCity1 = getCity1Route(map->route, routeId);
    City *tempCity2 = getCity2Route(map->route, routeId);
    City *prevCity = NULL;

    if (tempCity1 == NULL || tempCity2 == NULL) {
        return false;
    }

    while (tempCity1 != tempCity2) {
        City *nextCity = getNextInRoute(tempCity1, prevCity, routeId);
        Road *tempRoad = findRoad(tempCity1, nextCity);

        if (nextCity == NULL || tempRoad == NULL) {
            return false;
        }

        removeCityRoute(tempCity1, routeId);
        removeRoadRoute(tempRoad, routeId);

        prevCity = tempCity1;
        tempCity1 = nextCity;
    }

    removeCityRoute(tempCity2, routeId);
    removeSingleRoute(map->route, routeId);

    return true;
}