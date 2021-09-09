/** @file
 * Interface generatora losowych Id dla miast.
 */

#ifndef DROGI_RANDOMIDGENERATOR_H
#define DROGI_RANDOMIDGENERATOR_H

/** @brief Struktura generujaca unikalne pseudolosowe id.
 * Struktura generuje unikalne pseudolosowe identyfikatory
 * z przedzialu [0; 10000007).
 */
typedef struct RandomIdGen RandomIdGen;


/** @brief Funkcja tworzaca nowy generator.
 * Funkcja alokuje pamiec i tworzy nowy generator,
 * jesli sie nie udalo zwraca NULL.
 * @return Wskaznik na nowy generator, NULL jesli sie nie udalo
 */
RandomIdGen* createIdGen();

/** @brief Funkcja zwracajaca nowy identyfikator.
 * Funckja zwraca nowy, pseudolosowy, unikalny
 * indektyfikator z przedzialu [0; 10000007).
 * @param[in] argGen - generator
 * @return Nowe id
 */
unsigned newId(RandomIdGen* argGen);

/** @brief Funckja usuwajaca cala strukture.
 * Funckja usuwa z pamieci cala strukture i jej skladowe.
 * @param[in,out] argGen - usuwany generator
 */
void deleteGen(RandomIdGen **argGen);

#endif //DROGI_RANDOMIDGENERATOR_H
