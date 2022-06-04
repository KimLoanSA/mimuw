/** @file
 * Implemantacja listy.
 */

#include <stdlib.h>
#include <stdbool.h>
#include <inttypes.h>
#include <stdio.h>
#include "randomIdGenerator.h"
#include "citiesAndRoads.h"
#include "list.h"
#include "string.h"

/**
 * Struktura listy.
 */
struct List {
    List *nextElement; ///< pnastepny element
    List *prevELement; ///< poprzedni element
    City *cityPointer; ///< wskaznik na miasto
};


void addList(List **argList, City *argCIty) {
    if (argCIty == NULL) {
        return;
    }

    if (*argList == NULL) {
        *argList = malloc(sizeof(List));
        if (*argList == NULL) {
            return;
        }

        (*argList)->prevELement = NULL;
        (*argList)->nextElement = NULL;
        (*argList)->cityPointer = argCIty;
    }
    else {
        List *tempList = malloc(sizeof(List));
        if (tempList == NULL) {
            return;
        }

        tempList->prevELement = NULL;
        tempList->nextElement = (*argList);
        tempList->cityPointer = argCIty;

        (*argList)->prevELement = tempList;
        (*argList) = tempList;
    }
} //addList


City *findList(List *argList, const char *argCityName) {
    if (argList == NULL || argCityName == NULL) {
        return NULL;
    }

    if (compareString(argCityName, getCityName(argList->cityPointer))) {
        return argList->cityPointer;
    }

    return findList(argList->nextElement, argCityName);
} //findList


void deleteList(List **argList) {
    if (*argList != NULL) {
        deleteList(&((*argList)->nextElement));

        deleteCity(&((*argList)->cityPointer));
        free(*argList);
    }
} //deleteList


void resetAllElements(List *argList) {
    if (argList == NULL) {
        return;
    }
    resetCityInfo(argList->cityPointer);
    resetAllElements(argList->nextElement);
} //resetAllElements
