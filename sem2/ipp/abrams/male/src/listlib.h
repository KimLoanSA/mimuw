#ifndef _LISTLIB_H
#define _LISTLIB_H

//wskazniki na struktury
typedef struct RepresentantOfList *PointerRepresentantOfList;
typedef struct NodeOfList *ElementOfList;

//struktury
struct RepresentantOfList;
struct NodeOfList;

ElementOfList createList(uint64_t); //tworzy nowa liste inicjalizowana wartoscia
int deleteFromList(ElementOfList *); //usuwa z listy dany element
void
mergeLists(PointerRepresentantOfList, PointerRepresentantOfList); //laczy listy
int setValueOfRepresentant(ElementOfList *,
  uint64_t); //ustawia wartosc reprezentanta listy
uint64_t
getValueFromRepresentant(ElementOfList); //zwraca wartosc reprezentanta listy
uint32_t getSizeFromRepresentant(ElementOfList);  //zwraca rozmiar listy
PointerRepresentantOfList
getRepresentantOfList(ElementOfList); //zwraca wskaznik na reprezentanta listy

#endif //_LISTLIB_H

