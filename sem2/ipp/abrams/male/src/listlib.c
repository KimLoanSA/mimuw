#include <stdlib.h>
#include <inttypes.h>
#include "listlib.h"

//struktura reprezentanta listy
struct RepresentantOfList {
  uint64_t value; //wartosc reprezentanta
  uint32_t size; //rozmiar calej klasy
  ElementOfList beginingOfList, endOfList; //poczatek listy - skrajnie lewy element, koniec listy - skrajnie prawy element
};

//struktura wezla listy
struct NodeOfList {
  ElementOfList leftNeighbour, rightNeighbour; //sasiedzi
  PointerRepresentantOfList representant; //reprezentant
};

//tworzy i zwraca nowa liste
ElementOfList createList(uint64_t argumentInt) {
  ElementOfList resultElementOfList = NULL;
  resultElementOfList = (ElementOfList) malloc(sizeof(struct NodeOfList));

  PointerRepresentantOfList resultPointerRepresentantOfList = NULL;
  resultPointerRepresentantOfList = (PointerRepresentantOfList) malloc(
    sizeof(struct RepresentantOfList));

  if (resultElementOfList == NULL) { //blad w mallocu
    exit(1);
  }
  if (resultPointerRepresentantOfList == NULL) { //blad w mallocu
    exit(1);
  }

  resultElementOfList->leftNeighbour = NULL;
  resultElementOfList->rightNeighbour = NULL;
  resultElementOfList->representant = resultPointerRepresentantOfList;

  resultPointerRepresentantOfList->value = argumentInt;
  resultPointerRepresentantOfList->size = 1;
  resultPointerRepresentantOfList->beginingOfList = resultElementOfList;
  resultPointerRepresentantOfList->endOfList = resultElementOfList;

  return resultElementOfList;
} //createList

//usuwa z listy dany element
int deleteFromList(ElementOfList *argumentElementOfList) {
  if (*argumentElementOfList == NULL) { //zly argument
    return -1;
  }

  PointerRepresentantOfList tempPointerRepresentantOfList = getRepresentantOfList(
    *argumentElementOfList);
  ElementOfList tempLeftNeighbour = (*argumentElementOfList)->leftNeighbour;
  ElementOfList tempRightNeighbour = (*argumentElementOfList)->rightNeighbour;
  (tempPointerRepresentantOfList->size)--;

  if (tempLeftNeighbour != NULL) {
    tempLeftNeighbour->rightNeighbour = tempRightNeighbour;
  }
  if (tempRightNeighbour != NULL) {
    tempRightNeighbour->leftNeighbour = tempLeftNeighbour;
  }

  if (tempPointerRepresentantOfList->size ==
    0) { //usuwamy ostatni element z klasy
    free(tempPointerRepresentantOfList);
    tempPointerRepresentantOfList = NULL;
  } else {
    if (tempPointerRepresentantOfList->beginingOfList ==
      *argumentElementOfList) {
      tempPointerRepresentantOfList->beginingOfList = tempLeftNeighbour;
    }

    if (tempPointerRepresentantOfList->endOfList == *argumentElementOfList) {
      tempPointerRepresentantOfList->endOfList = tempRightNeighbour;
    }
  }

  free(*argumentElementOfList);
  *argumentElementOfList = NULL;

  return 0;
} //deleteFromList

//podpina druga liste do pierwszej
void mergeLists(PointerRepresentantOfList argument1PointerRepresentantOfList,
  PointerRepresentantOfList argument2PointerRepresentantOfList) {
  ElementOfList tempEndOfList1 = argument1PointerRepresentantOfList->endOfList;
  ElementOfList tempBeginingOfList2 = argument2PointerRepresentantOfList->beginingOfList;

  for (ElementOfList tempElementOfList = argument2PointerRepresentantOfList->endOfList;
    tempElementOfList !=
      NULL; tempElementOfList = tempElementOfList->rightNeighbour) {
    tempElementOfList->representant = argument1PointerRepresentantOfList;
  }

  tempBeginingOfList2->rightNeighbour = tempEndOfList1;
  tempEndOfList1->leftNeighbour = tempBeginingOfList2;

  argument1PointerRepresentantOfList->size += argument2PointerRepresentantOfList->size;
  argument1PointerRepresentantOfList->endOfList = argument2PointerRepresentantOfList->endOfList;

  free(argument2PointerRepresentantOfList);
  argument2PointerRepresentantOfList = NULL;
} //mergeLists

//zwraca wartosc reprezentanta listy, [0] jesli blad
uint64_t getValueFromRepresentant(ElementOfList argumentElementOfList) {
  if (argumentElementOfList == NULL) {
    return 0;
  }

  return argumentElementOfList->representant->value;
} //getValueFromRepresentant

//zwraca rozmiar listy, [0] jesli blad
uint32_t getSizeFromRepresentant(ElementOfList argumentElementOfList) {
  if (argumentElementOfList == NULL) {
    return 0;
  }

  return argumentElementOfList->representant->size;
} //getSizeFromRepresentant

//ustawia wartosc reprezentanta
int setValueOfRepresentant(ElementOfList *argumentElementOfList,
  uint64_t argumentInt) {
  if (*argumentElementOfList == NULL) {
    *argumentElementOfList = createList(argumentInt);

    if (*argumentElementOfList == NULL) { //blad w mallocu
      exit(1);
    }
  }

  (*argumentElementOfList)->representant->value = argumentInt;

  return 0;
} //setValueOfRepresentant

//zwraca reprezentanta listy, [null] jesli blad
PointerRepresentantOfList
getRepresentantOfList(ElementOfList argumentElementOfList) {
  if (argumentElementOfList == NULL) {
    return NULL;
  }

  return argumentElementOfList->representant;
} //getRepresentantOfList
