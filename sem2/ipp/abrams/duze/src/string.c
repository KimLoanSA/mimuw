/** @file
 * Implematacja klasy wykonujacej operacje na stringach.
 */
#include <stdlib.h>
#include <stdbool.h>
#include "string.h"

unsigned stringLength(const char *argString) {
    if (argString == NULL) {
        return 0;
    }

    unsigned res = 0;
    for (; *(argString + res) != '\0'; res++);

    return res;
} //stringLength


unsigned numberLength(unsigned argNumber) {
    if (argNumber == 0) {
        return 1;
    }

    unsigned res = 0;
    while (argNumber) {
        res++;
        argNumber /= 10;
    }

    return res;
} //numberLength


char *numberToString(unsigned argNumber) {
    unsigned length = numberLength(argNumber);
    char *resString = malloc(sizeof(char) * (length + 1));
    if (resString == NULL) {
        return NULL;
    }

    for (unsigned i = 0; i < length; i++) {
        *(resString + length - i - 1) = (char)('0' + (argNumber % 10));
        argNumber /= 10;
    }

    *(resString + length) = '\0';
    return resString;
} //numberToString


bool compareString(const char *argString1, const char *argString2) {
    if (stringLength(argString1) != stringLength(argString2)) {
        return false;
    }

    for (unsigned i = 0; *(argString1 + i) != '\0' && *(argString2 + i) != '\0'; i++) {
        if (*(argString1 + i) != *(argString2 + i)) {
            return false;
        }
    }

    return true;
} //compareString


bool checkString(const char *argString) {
    if (argString == NULL) {
        return false;
    }

    if (*argString == '\0') {
        return false;
    }

    for (unsigned i = 0; *(argString + i) != '\0'; i++) {
        if ((*(argString + i) > 0 && *(argString + i) <= 31) || *(argString + i) == ';') {
            return false;
        }
    }

    return true;
} //checkString