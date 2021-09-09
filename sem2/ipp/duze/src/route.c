/** @file
 * Implementacja klasy reprezentujacej drogi krajowe.
 */

#include <stdlib.h>
#include <stdbool.h>
#include <inttypes.h>
#include "bitset.h"
#include "randomIdGenerator.h"
#include "citiesAndRoads.h"
#include "route.h"

/**
 * Struktura przechowujaca pojedyncza droge krajowa.
 */
typedef struct SingleRoute {
    unsigned id; ///< id drogi krajowej
    City *city1; ///< poczatek drogi krajowej
    City *city2; ///< koniec drogi krajowej
} SingleRoute;

/**
 * Struktura przechowujaca drogi krajowe.
 */
struct Route {
    SingleRoute **routeTable; ///< tablica drog krajowych
    unsigned size; ///< rozmiar tablicy drog krajowych
};


/** @brief Funkcja tworzy nowa pojedyncza droge krajowa.
 * Funkcja alokuje pamiec i tworzy nowa droge krajowa,
 * zwraca NULL jesli alokowanie sie nie uda.
 * @param[in] argId - id tworzonej drogi
 * @param[in] argCity1 - poczatek tworzonej drogi
 * @param[in] argCity2 - koniec tworzonej drogi
 * @return Wskaznik na nowa droge krajowa, NULL jesli sie nie udalo
 */
SingleRoute *createSingleRoute(unsigned argId, City *argCity1, City *argCity2) {
    SingleRoute *resSingleRoute = malloc(sizeof(SingleRoute));
    if (resSingleRoute == NULL) { //alokacja sie nie powiodla
        return NULL;
    }

    resSingleRoute->id = argId;
    resSingleRoute->city1 = argCity1;
    resSingleRoute->city2 = argCity2;

    return  resSingleRoute;
} //createSingleRoute


Route *createRoute() {
    Route *resRoute = malloc(sizeof(Route));
    if (resRoute == NULL) { //alokacja sie nie powiodla
        return NULL;
    }

    resRoute->routeTable = malloc(sizeof(SingleRoute*) * 1000);
    if (resRoute->routeTable == NULL) { //alokacja sie nie powiodla
        return NULL;
    }

    resRoute->size = 1000;

    for (int i = 0; i < 1000; i++) {
        resRoute->routeTable[i] = NULL;
    }

    return resRoute;
} //createRoute


void addRoute(Route *argRoute, unsigned argId, City *argCity1, City *argCity2) {
    if (argRoute == NULL || argId >= argRoute->size || argId == 0 ||
        argCity1 == NULL || argCity2 == NULL) {
        return;
    }

    argRoute->routeTable[argId] = createSingleRoute(argId, argCity1, argCity2);

} //addRoute


bool findRoute(Route *argRoute, unsigned argId) {
    if (argRoute == NULL || argId >= argRoute->size ||
        argRoute->routeTable == NULL) {
        return false;
    }

    return argRoute->routeTable[argId];
} //findRoute


City *getCity1Route(Route *argRoute, unsigned argId) {
    if (argRoute == NULL || argId >= argRoute->size ||
        argRoute->routeTable[argId] == NULL) {
        return NULL;
    }

    return argRoute->routeTable[argId]->city1;
} //getCity1Route


City *getCity2Route(Route *argRoute, unsigned argId) {
    if (argRoute == NULL || argId >= argRoute->size ||
        argRoute->routeTable[argId] == NULL) {
        return NULL;
    }

    return argRoute->routeTable[argId]->city2;
} //getCity2Route


void editCityRoute(Route *argRoute, unsigned argId, City *argOldCity, City *argNewCity) {
    if (argRoute == NULL || argId >= argRoute->size ||
        argOldCity == NULL || argNewCity == NULL ||
        argRoute->routeTable[argId] == NULL) {
        return;
    }

    if (argRoute->routeTable[argId]->city1 == argOldCity) {
        argRoute->routeTable[argId]->city1 = argNewCity;
    }
    else if (argRoute->routeTable[argId]->city2 == argOldCity) {
        argRoute->routeTable[argId]->city2 = argNewCity;
    }
} //editCityRoute


void deleteRoute(Route **argRoute) {
    if (*argRoute == NULL) {
        return;
    }

    for (unsigned i = 0; i < (*argRoute)->size; i++) {
        free((*argRoute)->routeTable[i]);
    }

    free((*argRoute)->routeTable);
    free(*argRoute);
} //deleteRoute


void removeSingleRoute(Route *argRoute, unsigned argId) {
    if (argRoute == NULL) {
        return;
    }

    if (argId <= 0 || argId > 999) {
        return;
    }

    if (argRoute->routeTable[argId] != NULL) {
        free(argRoute->routeTable[argId]);
        argRoute->routeTable[argId] = NULL;
    }
}