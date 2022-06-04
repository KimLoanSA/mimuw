#ifndef _DSULIB_H
#define _DSULIB_H

int unionSets(ElementOfList *, ElementOfList *); //laczy dwie klasy abstrakcji
int setValueOfSet(ElementOfList *, uint64_t); //ustawia wartosc klasu abstrakcji
ElementOfList createNewSet(uint64_t); //tworzy nowa klase abstrakcji
uint32_t getSizeOfSet(ElementOfList); //zwraca rozmiar klasy abstrakcji
void removeElementOfSet(ElementOfList *); //usuwa element z klasy abstrakcji
uint64_t getValueOfSet(ElementOfList); //zwraca wartosc klasy abstrakcji

#endif //_DSULIB_H

