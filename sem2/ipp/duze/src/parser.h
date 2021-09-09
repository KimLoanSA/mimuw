/** @file
 * Interface parsera.
 */

#ifndef DROGI_PARSER_H
#define DROGI_PARSER_H

/** @brief Funkcja sprawdza czy zostal wczytany znak EOF
 * @return @p true, jesli zostal wczytany EOF, @p false wpp
 */
bool isEOF();

/** @brief Pominja jedna linie wejscia.
 */
void skipOneLine();

/** @brief Funkcja zwraca rodzaj operacji.
 * @return
 * @p 0, jesli nieprawidlowe wejscie
 * @p 1 - tworzenie drogi krajowej,
 * @p 2 - addRoad
 * @p 3 - repairRoad
 * @p 4 - getRouteDescription
 * @p 5 - koniec pliku
 * @p 6 - komentarz / nowa linia
 * @p 7 - newRoute
 * @p 8 - extendRoute
 * @p 9 - removeRoad
 * @p 10 - removeRoute
 */
int getTypeOfOperation();

/** @brief Funckja zwraca liczbe z wejscia.
 * Funkcja zwraca liczbe z wejscia, jesli przekracza
 * zakres zwraca 0.
 * @return Liczba z wejscia, @p 0 jesli overflow
 */
int64_t getNumber();


/** @brief Funcja wypisuje na strumien diagnosryczny error.
 */
void raiseError();


/** @brief Funkcja zwraca stringa z wejscia.
 * @return Wskaznik na napis, NULL jesli sie nie powiodlo
 */
char* getString();


/** @brief Funkcja sprawdza czy nastepny znak jest srednikiem.
 * @return @p true, jesli nastepny znak jest srdenikiem, @p false wpp
 */
bool isSemicolon();


/** @brief Funkcja sprawdza czy nastepny znak jest znakiem nowej linii.
 * @return @p true, jesli nastepny znak jest znakiem nowej linii, @p false wpp
 */
bool isNewLine();


/** @brief Funkcja sprawdza czy nstapeny znak jest cyfra.
 * @return @p true jesli nastepny znak jest cyfra, @p false wpp
 */
bool isDigitNext();


#endif //DROGI_PARSER_H
