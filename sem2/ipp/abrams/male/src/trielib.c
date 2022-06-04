#include <stdlib.h>
#include <inttypes.h>
#include "iolib.h"
#include "listlib.h"
#include "dsulib.h"
#include "trielib.h"

//struktura reprezentujaca wierzcholek drzewa
struct NodeOfTree {
  PointerOnTree subTree[4]; //wskazniki na poddrzewa
  ElementOfList pointerOnSet; //wskaznik na element listy
};

//tworzy i zwraca nowe drzewo
PointerOnTree createTree() {
  PointerOnTree tempPointerOnTree = NULL;
  tempPointerOnTree = (PointerOnTree) malloc(sizeof(struct NodeOfTree));

  if (tempPointerOnTree == NULL) { //blad w mallocu
    exit(1);
  }

  tempPointerOnTree->pointerOnSet = createList(0);

  for (int i = 0; i < 4; i++) {
    tempPointerOnTree->subTree[i] = NULL;
  }

  return tempPointerOnTree;
} //createTree

//usuwa poddrzewo
void removeSubTree(PointerOnTree *argumentPointerOnTree) {
  if (*argumentPointerOnTree != NULL) {
    for (int i = 0; i < 4; i++) {
      removeSubTree(&((*argumentPointerOnTree)->subTree[i]));
    }

    removeElementOfSet(&((*argumentPointerOnTree)->pointerOnSet));

    (*argumentPointerOnTree)->pointerOnSet = NULL;
    free(*argumentPointerOnTree);
    *argumentPointerOnTree = NULL;
  }
} //removeSubTree

//usuwa z drzewa kazdy ciag posiadajacy dany prefiks
int removeFromTree(PointerOnTree *argumentPointerOnTree) {
  int actualElementOfSequence = getOneElementOfSequence();

  if (actualElementOfSequence == -1) { //zly znak
    return -1;
  } else if (actualElementOfSequence == 4) { //koniec ciagu
    if (isEndOfLine()) { //ciag musi sie konczyc znakiem nowej linii
      removeSubTree(argumentPointerOnTree);
      return 0;
    } else {
      return -1;
    }
  } else if ((*argumentPointerOnTree)->subTree[actualElementOfSequence] ==
    NULL) {
    return checkLine(); //sprawdzanie reszty ciagu w poszukiwaniu zlych znakow
  } else {
    return removeFromTree(
      &((*argumentPointerOnTree)->subTree[actualElementOfSequence]));
  }
} //removeFromTree

//dodaje do drzewa dany ciag
int addToTree(PointerOnTree *argumentPointerOnTree) {
  if (*argumentPointerOnTree == NULL) {
    return -1;
  }

  int actualElementOfSequence = getOneElementOfSequence();

  if (actualElementOfSequence == 4) { //koniec ciagu
    if (isEndOfLine()) { //ciag musi sie konczyc znakiem nowej linii
      return 0;
    } else {
      return -1;
    }
  } else if (actualElementOfSequence == -1) { //zly element
    return -1;
  } else if ((*argumentPointerOnTree)->subTree[actualElementOfSequence] ==
    NULL) {
    (*argumentPointerOnTree)->subTree[actualElementOfSequence] = createTree();

    if (*argumentPointerOnTree == NULL) {
      exit(1);
    }
    //jesli dodalismy do drzewa ciag, ktory zawiera niewlasciwe znaki musimy go usunac
    if (addToTree(
      &((*argumentPointerOnTree)->subTree[actualElementOfSequence])) == -1) {
      removeSubTree(
        &((*argumentPointerOnTree)->subTree[actualElementOfSequence]));

      return -1;
    }
  } else {
    return addToTree(
      &((*argumentPointerOnTree)->subTree[actualElementOfSequence]));
  }
  return 0;
} //addToTree

//usuwa drzewo
void deleteTree(PointerOnTree *argumentPointerOnTree) {
  removeSubTree(argumentPointerOnTree);
} //deleteTree

//zwraca wskaznik na liste dla danego ciagu
ElementOfList *getSetFromTree(PointerOnTree argumentPointerOnTree) {
  if (argumentPointerOnTree == NULL) {
    return NULL;
  }

  int actualElementOfSequence = getOneElementOfSequence();

  if (actualElementOfSequence == -1) { //zly element
    return NULL;
  } else if (actualElementOfSequence == 4) { //koniec ciagu
    return &(argumentPointerOnTree->pointerOnSet);
  }
  if (argumentPointerOnTree->subTree[actualElementOfSequence] == NULL) {
    return NULL;
  }

  return getSetFromTree(
    argumentPointerOnTree->subTree[actualElementOfSequence]);
} //getSetFromTree

//znajduje dany ciag w drzewie
int findInTree(PointerOnTree argumentPointerOnTree) {
  int actualElementOfSequence = getOneElementOfSequence();

  if (actualElementOfSequence == -1) { //zly element
    return -1;
  } else if (argumentPointerOnTree == NULL) {
    return checkLine(); //sprawdzanie reszty ciagu w poszukiwaniu zlych znakow
  } else if (actualElementOfSequence == 4) { //koniec ciagu
    if (isEndOfLine()) {
      return 1;
    } else {
      return -1;
    }
  }


  return findInTree(argumentPointerOnTree->subTree[actualElementOfSequence]);
} //findInTree
