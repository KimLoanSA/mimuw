/** @file
 * Implementaca generatora losowych Id dla miast.
 */
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <stdbool.h>
#include <inttypes.h>
#include "bitset.h"
#include "randomIdGenerator.h"

/** @brief Struktura generujaca unikalne pseudolosowe id.
 * Struktura generuje unikalne pseudolosowe identyfikatory
 * z przedzialu [0; 10000007).
 */
struct RandomIdGen {
    Bitset *used; ///<bitset uzytych wartosci
};



RandomIdGen* createIdGen() {
    srand(2250 * 1822);

    RandomIdGen *resGen = malloc(sizeof(RandomIdGen));
    if (resGen == NULL) { //alokacja sie nie powiodla
        return NULL;
    }

    resGen->used = createBitset(10000007);

    return resGen;
} //createIdGen

/** @brief Funkcja zwraca pseudolosowa liczbe z przedzialu [0; 10000007).
 * @return Pseudolosowa liczba.
 */
unsigned randomValue() {
    unsigned resValue = 0;

    for (unsigned i = 0; i < 32; i++) {
        if (rand() % 2) {
            resValue |= (1U << i);
        }
    }

    return resValue % 10000007;
} //randomValue


unsigned newId(RandomIdGen* argGen) {
    if (argGen == NULL) {
        return 0;
    }

    unsigned resValue = randomValue();

    while (findBitset(argGen->used, resValue)) {
        resValue = randomValue();
    }

    addBitset(argGen->used, resValue);

    return resValue;
} //newId


void deleteGen(RandomIdGen **argGen) {
    if (*argGen == NULL) {
        return;
    }

    deleteBitset(&((*argGen)->used));

    free(*argGen);
} //deleteGen
