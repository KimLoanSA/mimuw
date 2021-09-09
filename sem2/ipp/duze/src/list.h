/** @file
 * Interface listy.
 */

#ifndef DROGI_LIST_H
#define DROGI_LIST_H

/** @brief Struktura reprezentujaca liste.
 */
typedef struct List List;

/** @brief Funkcja dodajaca do listy miasto.
 * Funkcja dodaje do listy miasto, jesli
 * wskaznik na miasto to NULL
 * wtedy nic nie robi.
 * @param[in,out] argList - modyfikowana lista
 * @param[in] argCIty - dodawane miasto
 */
void addList(List **argList, City *argCIty);

/** @brief Funkcja znajdujaca miasto o danej nazwie.
 * Funkcja znajduje w liscie miasto o
 * danej nazwie, jesli takiego nie ma zwraca NULL;
 * @param[in] argList - przeszukiwana lista
 * @param[in] argCityName - nazwa szukanego miasta
 * @return Wskaznik na szukane miasto, NULL jesli nie znaleziono
 */
City *findList(List *argList, const char *argCityName);

/** @brief Funkcja restetujaca kazde z miast.
 * Funkcja resetuje informacje o kazdym miescie,
 * znajdujacym sie w niej.
 * @param[in] argList - dana lista
 */
void resetAllElements(List *argList);

/** @brief Funkcja usuwajaca cala strukture.
 * Funkcja usuwajaca z pamieci cala liste i jej skladniki.
 * @param[in,out] argList - usuwana lista
 */
void deleteList(List **argList);

#endif //DROGI_LIST_H
