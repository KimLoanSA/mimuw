/** @file
 * Implementacja interface'a parsera.
 */

#include <stdlib.h>
#include <stdbool.h>
#include <ctype.h>
#include <stdio.h>
#include <inttypes.h>
#include "string.h"
#include "parser.h"

/** Globalna zmienna liczaca linie wejscia.
 */
uint64_t lineNumber = 0;

bool isEOF() {
    return (bool)feof(stdin);
} //isEOF


void skipOneLine() {
    while (getchar() != '\n' && isEOF() == 0);
} //skipOneLine


int getTypeOfOperation() {
    //skipLines();
    int tempChar = getchar();
    lineNumber++;

    if (feof(stdin)) { //koniec pliku
        return 5;
    }

    if (isdigit(tempChar)) {
        ungetc(tempChar, stdin);
        return 1;
    }

    if (tempChar == '\n') {
        ungetc(tempChar, stdin);
        return 6;
    }

    if (tempChar == '#') {
        return 6;
    }

    char *tempTab = malloc(sizeof(char) * 22);
    if (tempTab == NULL) { //blad w mallocu
        return 0;
    }

    int counter;
    for (counter = 0; counter < 21 && tempChar != ';' && tempChar != '\n' && isEOF() == false; counter++) {
        *(tempTab + counter) = (char)tempChar;
        tempChar = getchar();
    }
    *(tempTab + counter) = '\0';
    ungetc(tempChar, stdin);

    if (counter == 7 && compareString((const char*)tempTab, "addRoad")) {
        free(tempTab);

        return 2;
    }

    if (counter == 10 && compareString((const char*)tempTab, "repairRoad")) {
        free(tempTab);

        return 3;
    }

    if (counter == 19 && compareString((const char*)tempTab, "getRouteDescription")) {
        free(tempTab);

        return 4;
    }

    if (counter == 8 && compareString((const char*)tempTab, "newRoute")) {
        free(tempTab);

        return 7;
    }

    if (counter == 11 && compareString((const char*)tempTab, "extendRoute")) {
        free(tempTab);

        return 8;
    }

    if (counter == 10 && compareString((const char*)tempTab, "removeRoad")) {
        free(tempTab);

        return 9;
    }

    if (counter == 11 && compareString((const char*)tempTab, "removeRoute")) {
        free(tempTab);

        return 10;
    }


    free(tempTab);

    return 0;
} //getTypeOfOperation


int64_t getNumber() {
    int64_t resInt = 0;
    int actChar = getchar();
    bool isNegative = false;

    if (actChar == '-') {
        isNegative = true;
    }
    else {
        ungetc(actChar, stdin);
    }

    for (actChar = getchar(); actChar != '\n' &&
        actChar != ';' && isEOF() == 0; actChar = getchar()) {
        if (isdigit(actChar)) {
            int tempInt = actChar - '0';

            if (((INT64_MAX - (int64_t)tempInt) / 10LL) < resInt) {
                ungetc(actChar, stdin);

                return 0;
            }

            resInt = resInt * 10LL + tempInt;
        }
        else {
            ungetc(actChar, stdin);

            return 0;
        }
    }

    ungetc(actChar, stdin);

    if (isNegative) {
        resInt *= -1LL;
    }

    return resInt;
} //getNumber


void raiseError() {
    fprintf(stderr, "ERROR %"PRIu64"\n", lineNumber);
} //raiseError


char* getString() {
    unsigned stringIndex = 0;
    unsigned stringSize = 2;
    char *resString = malloc(sizeof(char) * stringSize);
    int actChar;

    for (actChar = getchar(); actChar != '\n' &&
                              actChar != ';' && isEOF() == 0; actChar = getchar()) {
        if (actChar > 31) {
            if (stringIndex == stringSize) {
                stringSize *= 2;

                resString = realloc(resString, stringSize);
                if (resString == NULL) {
                    return NULL;
                }
            }

            *(resString + stringIndex) = (char)actChar;
            stringIndex++;
        }
        else {
            free(resString);
            return NULL;
        }
    }
    ungetc(actChar, stdin);
    resString = realloc(resString, stringIndex + 1);
    *(resString + stringIndex) = '\0';

    return resString;
} //getString


bool isSemicolon() {
    int tempChar = getchar();

    bool res = tempChar == ';';

    if (tempChar == '\n')
        ungetc(tempChar, stdin);

    return res;
} //isSemicolon


bool isNewLine() {
    int tempChar = getchar();

    bool res = tempChar == '\n';
    ungetc(tempChar, stdin);

    return res;
} //isNewLine


bool isDigitNext() {
    int tempChar = getchar();

    bool res = (bool)isdigit(tempChar);
    ungetc(tempChar, stdin);

    return res;
} //isDigitNext