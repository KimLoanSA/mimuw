/** @file
 * Interface klasy wykonujacej
 * operacje na stringach
 */

#ifndef DROGI_STRING_H
#define DROGI_STRING_H

/** @brief Funcja konwertujaca liczbe do stringa.
 * Funckja zwraca liczbe zapisana w stringu
 * lub NULL jesli sie nie udalo zaalokowac pamieci.
 * @param[in] argNumber - konwertowana liczba
 * @return Wskaznik na stringa
 */
char *numberToString(unsigned argNumber);

/** @brief Funckja zwracajaca dlugosc stringa.
 * @param[in] argString - dany string
 * @return Dlugosc danego stringa
 */
unsigned stringLength(const char *argString);

/** @brief Funkcja zwraca dlugosc liczby.
 * @param argNumber - dana liczba
 * @return Dlugosc danej liczby
 */
unsigned numberLength(unsigned argNumber);

/** @brief Funkcja porownujaca dwa stringi.
 * @param[in] argString1 - porownywany string nr 1.
 * @param[in] argString2 - porownywany string nr 2.
 * @return @p true, jesli stringi sa takie same, @p false wpp
 */
bool compareString(const char *argString1, const char *argString2);

/** @brief Funkcja sprawdzajaca poprawnosc stringa.
 * Funkcja sprawdza, czy string jest niepusty
 * oraz czy nie zawiera znakow mniejszych rownych 31.
 * @param[in] argString - sprawdzyny string
 * @return @p true, jesli string jest poprawny, @p false wpp
 */
bool checkString(const char *argString);

#endif //DROGI_STRING_H
