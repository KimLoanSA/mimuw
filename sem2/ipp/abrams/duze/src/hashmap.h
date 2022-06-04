/** @file
 * Interface klasy hashmapy.
 */

#ifndef DROGI_HASHMAP_H
#define DROGI_HASHMAP_H

/** @brief Sturuktura reprezentujaca hashmape.
 *  Hashmapa, ktora obsluguje nazwy miast.
 */
typedef struct Hashmap Hashmap;


/** @brief Funkcja tworzaca nowa hashmape.
 * Funkcja alokuje pamiec i tworzy nowa hashmape,
 * jesli alokowanie sie nie uda, zwraca NULL.
 * @return Wskaznik na nowa hashmape, NULL jesli sie nie udalo
 */
Hashmap *createHashmap();

/** @brief Funkcja dodajaca do hashmapy miasto.
 * Funcka hashuje nazwe miasta i pod
 * odpowiednim indeksem umieszcza miasto
 * na liscie.
 * @param[in,out] argHashmap - edytowana hashmapa
 * @param[in] argCity - dodawane miasto
 */
void addHashmap(Hashmap *argHashmap, City *argCity);

/** @brief Funkcja znajdujaca miasto o danej nazwie.
 * Funkcja znajduje miasto do danej nazwie w hashmapie
 * jesli nie istnieje to zwraca NULL.
 * @param[in] argHashmap - przeszukiwana hashmapa
 * @param[in] argCityName - nazwa szukanego miasta
 * @return Wskaznik na iasto o danej nazwie, NULL jesli nie znaleziono
 */
City *findHashmap(Hashmap *argHashmap, const char *argCityName);

/** @brief Funkcja resetujaca wszystkie miasta w hashmapie.
 * Funkcja resetuje informacje o wszysykich miastach
 * (informacja potrzebne dla SPFA).
 * @param[in,out] argHashmap - reserowana hashmapa
 */
void resetAllHashmap(Hashmap *argHashmap);

/** @brief Funkcja usuwajaca cala strukture.
 * Funckja usuwa z pamieci cala strukture i jej skladowe.
 * @param[in,out] argHashmap - usuwana hashmapa
 */
void deleteHashmap(Hashmap **argHashmap);

#endif //DROGI_HASHMAP_H
