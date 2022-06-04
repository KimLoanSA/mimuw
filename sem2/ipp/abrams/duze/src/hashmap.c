/** @file
 * Implementacja klasy hashmap.
 */

#include <stdlib.h>
#include <stdbool.h>
#include <inttypes.h>
#include "bitset.h"
#include "randomIdGenerator.h"
#include "citiesAndRoads.h"
#include "list.h"
#include "hashmap.h"
#include "string.h"

/**
 * Struktura reprezentujaca hashmape.
 */
struct Hashmap {
    List **hashmap; ///< tablica list dla haszy
    unsigned size; ///< rozmiar hashmapy
};


/** @brief Funckaj haszujaca dany string.
 * Funckja haszuje dany string
 * z podstawa = 251 i modulo = 1000999.
 * @param[in] argString - haszowany string
 * @return Hasz danego stringa
 */
unsigned hashString(const char *argString) {
    unsigned resultValue = 0;
    const unsigned prime = 251; //podstawa hasha
    const unsigned modulo = 1000999; //modulo, czyli rozmiar hashmapy

    for (int i = 0; *(argString + i) != '\0'; i++) {
        resultValue = ((resultValue * prime) % modulo +  (uint8_t) *(argString + i)) % modulo;
    }

    return resultValue;
} //hashString


Hashmap *createHashmap() {
    Hashmap *resHashmap = malloc(sizeof(Hashmap));
    if (resHashmap == NULL) {
        return NULL;
    }

    resHashmap->size = 1000999;
    resHashmap->hashmap = malloc(sizeof(List*) * 1000999);
    if (resHashmap->hashmap == NULL) {
        return NULL;
    }

    for (unsigned i = 0; i < resHashmap->size; i++) {
        resHashmap->hashmap[i] = NULL;
    }

    return resHashmap;
} //createHashmap

void addHashmap(Hashmap *argHashmap, City *argCity) {
    if (argHashmap == NULL || argCity == NULL) {
        return;
    }

    unsigned cityNameHash = hashString(getCityName(argCity));

    addList(&(argHashmap->hashmap[cityNameHash]), argCity);
} //addHashmap


City *findHashmap(Hashmap *argHashmap, const char *argCityName) {
    if (argHashmap == NULL || argCityName == NULL) {
        return NULL;
    }

    return findList(argHashmap->hashmap[hashString(argCityName)], argCityName);
} //findHashmap


void resetAllHashmap(Hashmap *argHashmap) {
    for (unsigned i = 0; i < argHashmap->size; i++) {
        resetAllElements(argHashmap->hashmap[i]);
    }
} //resetAllHashmap


void deleteHashmap(Hashmap **argHashmap) {
    if (*argHashmap == NULL) {
        return;
    }

    for (unsigned i = 0; i < (*argHashmap)->size; i++) {
        deleteList(&((*argHashmap)->hashmap[i]));
    }

    free((*argHashmap)->hashmap);
    free(*argHashmap);
} //deleteHashmap
