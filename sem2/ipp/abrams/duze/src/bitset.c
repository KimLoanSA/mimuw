/** @file
 * Implemantacja klasy bitset.
 */

#include <stdlib.h>
#include <inttypes.h>
#include <stdbool.h>
#include "bitset.h"

/**
 * Struktura reprezentujaca bitset.
 */
struct Bitset {
    unsigned maxVal; ///< maksymalna wartosc w bitsecie
    unsigned *bitTable; ///< tablica na bity
};



Bitset* createBitset(unsigned argValue) {
    Bitset* resBitset = malloc(sizeof(Bitset));
    if (resBitset == NULL) { //alokowanie sie nie udalo
        return NULL;
    }

    resBitset->maxVal = argValue;
    unsigned tempSize = argValue / 32 + 1;

    unsigned *tempTable = malloc(sizeof(unsigned) * tempSize);
    if (tempTable == NULL) { //alokowanie sie nie udalo
        return NULL;
    }

    for (unsigned i = 0; i < tempSize; i++) {
        tempTable[i] = 0;
    }

    resBitset->bitTable = tempTable;

    return resBitset;
} //createBitset


void addBitset(Bitset *argBitset, unsigned argNumber) {
    if (argBitset == NULL || argNumber > argBitset->maxVal) {
        return;
    }

    unsigned tempBit = (1U << (argNumber % 32));
    argBitset->bitTable[argNumber / 32] |= tempBit;
} //addBitset


bool findBitset(Bitset *argBitset, unsigned argNumber) {
    if (argBitset == NULL || argNumber > argBitset->maxVal) {
        return NULL;
    }

    unsigned tempBit = (1U << (argNumber % 32));
    unsigned tempMask = (argBitset->bitTable[argNumber / 32]);

    return (bool)(tempBit & tempMask);
} //findBitset


void removeBitset(Bitset *argBitset, unsigned argNumber) {
    if (argBitset == NULL || argNumber > argBitset->maxVal) {
        return;
    }

    unsigned tempBit = ~(1U << (argNumber % 32));
    argBitset->bitTable[argNumber / 32] &= tempBit;
} //removeBitset


void deleteBitset(Bitset **argBitset) {
    if (*argBitset == NULL) {
        return;
    }

    free((*argBitset)->bitTable);
    free(*argBitset);
    (*argBitset) = NULL;
} //deleteBitset
