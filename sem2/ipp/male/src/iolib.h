#ifndef _IOLIB_H
#define _IOLIB_H

void skipOneLine(); //pomija jedna linie
void skipLines(); //pomija puste linie i zaczynajace sie od #
void outputAnswer(int); //wypisuje OK/ERROR
void outputNumber(uint64_t); //wypisuje liczbe/ERROR
void outputAnswerYN(int); //wypisuje YES/NO/ERROR
int getOneElementOfSequence(); //zwraca kolejny element z ciagu wejsciowego
int getTypeOfOperation(); //zwraca rodzaj operacji
uint64_t getNumber(); //zwraca liczbe z wejscia
int checkLine(); //sprawdza linie, czy wszystkie znaki sa dobre
int isEndOfLine(); //sprawdza czy jest to koniec linii	
int nextChar(); //sprawdza nastepny znak
void printError(); //wypisuje ERROR na wyjscie diagnostyczne
void skipOneChar(); //pomija jedna znak z wejscia
uint64_t getDigit(char); //zwraca cyfre
int checkNextCharForNumber(); //sprawdza czy nastepny znak jest cyfra
int isEOF(); //sprawdza czy jest to koniec pliku

#endif //_IOLIB_H
