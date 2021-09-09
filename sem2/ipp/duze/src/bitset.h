/** @file
 * Interfejs bitsetu
 */

#ifndef DROGI_BITSET_H
#define DROGI_BITSET_H

/** @brief Struktura przechowujaca bitset.
 * Struktura zawiera tablice intow,
 * odpowiedznie bity mowia czy element
 * jest w zbiorze czy go nie ma
 */
typedef struct Bitset Bitset;


/** @brief Funkcja towrzaca nowy bitset o zadanej wielkosci (maksymalna liczba).
 * Funkcja alkouje pamiec i zwraca wskaznik na strukture.
 * @param[in] argValue - rozmiar towrzonego bitseta (maksymalna liczba, ktora chcemy przechowywac)
 * @return Wskaznik na nowy bitset, NULL jesli sie nie udalo
 */
Bitset *createBitset(unsigned argValue);

/** @brief Funkcja dodajaca do bitseta wartosc.
 * Funckja dodaje do bitseta dana wartosc jesli miesci sie
 * w przeciale [0; maxVal]
 * w przeciwnym przypadku nic nie robi.
 * @param[in,out] argBitset - modyfikowany bitset
 * @param[in] argNumber - wartosc dodawana
 */
void addBitset(Bitset *argBitset, unsigned argNumber);

/** @brief Funkcja szukajaca danej wartosci w bitsecie.
 * Funkcja szuka danej wartosci w bitsecie,
 * jesli danej wartosci nie ma, badz przekracza rozmiar
 * bitseta funkcja zwraca false.
 * @param[in] argBitset - przeszukiwany bitset
 * @param[in] argNumber - szukana wartosc
 * @return @p true, jesli wartosc jest w bitsecie, @p false wpp
 */
bool findBitset(Bitset *argBitset, unsigned argNumber);


/** @brief Funkcja usuwa dana wartosc z bitsetu.
 * @param[in,out] argBitset - edytowany bitset
 * @param[in] argNumber - usuwana wartosc
 */
void removeBitset(Bitset *argBitset, unsigned argNumber);

/** @brief Funkcja usuwajaca cala strukture.
 * Funcjka usuwa z pamieci cala strukture i jej skladowe.
 * @param[in,out] argBitset - usuwany bitset
 */
void deleteBitset(Bitset **argBitset);

#endif //DROGI_BITSET_H
