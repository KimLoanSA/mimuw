#include <inttypes.h>
#include "listlib.h"
#include "dsulib.h"

//zwraca rozmiar klasy abstrakcji
uint32_t getSizeOfSet(ElementOfList argumentElementOfList) {
  return getSizeFromRepresentant(argumentElementOfList);
} //getSizeOfSet

//zwraca wartosc klasy abstrakcji
int setValueOfSet(ElementOfList *argumentElementOfList, uint64_t argumentInt) {
  return setValueOfRepresentant(argumentElementOfList, argumentInt);
} //setValueOfSet

//tworzy nowa klase
ElementOfList createNewSet(uint64_t argumentInt) {
  return createList(argumentInt);
} //createNewSet

//laczy klasy abstrakcji (mniejsze do wiekszego)
int unionSets(ElementOfList *argument1Set, ElementOfList *argument2Set) {
  if (*argument1Set == *argument2Set ||
    getRepresentantOfList(*argument1Set) ==
      getRepresentantOfList(*argument2Set)) {
    return 0;
  }

  uint64_t valueOfSet1 = getValueFromRepresentant(*argument1Set);
  uint64_t valueOfSet2 = getValueFromRepresentant(*argument2Set);

  if (valueOfSet1 == 0 && valueOfSet2 == 0) { //obie klasy nie istnieja
    return -1;
  } else if (valueOfSet1 == 0) { //pierwsza klasa jest pusta, wiec dolaczamy ja
    mergeLists(getRepresentantOfList(*argument2Set),
      getRepresentantOfList(*argument1Set));
  } else if (valueOfSet2 == 0) { //druga klasa jest pusta, wiec dolaczamy ja
    mergeLists(getRepresentantOfList(*argument1Set),
      getRepresentantOfList(*argument2Set));
  } else {
    //obliczanie sredniej bez przekrecania
    uint64_t tempAverageValueOfSets = valueOfSet1 / 2 + valueOfSet2 / 2 +
      (valueOfSet1 % 2 + valueOfSet2 % 2) / 2;

    if (getSizeOfSet(*argument1Set) >
      getSizeOfSet(*argument2Set)) { //podlaczamy mniejesza do wiekszej
      mergeLists(getRepresentantOfList(*argument1Set),
        getRepresentantOfList(*argument2Set));
    } else {
      mergeLists(getRepresentantOfList(*argument2Set),
        getRepresentantOfList(*argument1Set));
    }
    setValueOfRepresentant(argument1Set, tempAverageValueOfSets);
  }

  return 0;
} //unionSets

//usuwa element z klasy abstrakcji
void removeElementOfSet(ElementOfList *argumentElementOfList) {
  deleteFromList(argumentElementOfList);
} //removeElementOfSet

//zwraca wartosc klasy abstrakcji
uint64_t getValueOfSet(ElementOfList argumentElementOfList) {
  return getValueFromRepresentant(argumentElementOfList);
} //getValueOfSet
