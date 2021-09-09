#ifndef _TRIELIB_H
#define _TRIELIB_H

//wskaznik na strukture
typedef struct NodeOfTree *PointerOnTree;

//struktura
struct NodeOfTree;

int addToTree(PointerOnTree *); //dodawanie do drzewa
int removeFromTree(PointerOnTree *); //usuwanie z drzewa
ElementOfList *
getSetFromTree(PointerOnTree); //zwraca wskaznik na klase abstrakcji
void deleteTree(PointerOnTree *); //usuwanie drzewa
PointerOnTree createTree(); //tworzy i zwraca nowe drzewo
int findInTree(PointerOnTree); //wyszukiwanie w drzewie

#endif //_TRIELIB_H

