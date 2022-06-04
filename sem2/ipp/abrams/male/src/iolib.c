#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <ctype.h>
#include <inttypes.h>
#include "iolib.h"

//pomija jedna linie wejscia
void skipOneLine() {
  while (getchar() != '\n' && isEOF() == 0);
} //skipOneLine

//pomija puste linie i zaczynajace sie od #
void skipLines() {
  int actualChar;

  for (actualChar = getchar();
    actualChar == '\n' || actualChar == '#'; actualChar = getchar()) {
    if (actualChar == '#') {
      skipOneLine();
    }
  }

  ungetc(actualChar, stdin);
} //skipLines


int compareStrings(int *argumentTab1, int *argumentTab2, int argumentSize) {
  for (int i = 0; i < argumentSize; i++) {
    if (*(argumentTab1 + i) != *(argumentTab2 + i)) {
      return 0;
    }
  }

  return 1;
}

//zwraca element ciagu z wejscia {0,1,2,3}, [4] jesli to koniec ciagu, [-1] jesli blad
int getOneElementOfSequence() {
  int actualChar = getchar();

  if (isEOF()) {
    return -1;
  } else if (actualChar == '0') {
    return 0;
  } else if (actualChar == '1') {
    return 1;
  } else if (actualChar == '2') {
    return 2;
  } else if (actualChar == '3') {
    return 3;
  } else if (actualChar == '\n' || actualChar == ' ') {
    ungetc(actualChar, stdin);

    return 4;
  } else {
    return -1;
  }
} //getOneElementOfSequence

//zwraca rodzaj operacji:
//[-1]:		ERROR
//[1]:		DECLARE
//[2]:		REMOVE
//[3]:		VALID
//[4]:		ENERGY
//[5]:		EQUAL
//[6]: 		END
int getTypeOfOperation() {
  skipLines();
  int tempChar = getchar();

  if (feof(stdin)) { //koniec pliku
    return 6;
  }

  int counter = 0;
  int *tempTab = malloc(sizeof(int) * 10);
  int tabDECLARE[] = {'D', 'E', 'C', 'L', 'A', 'R', 'E'};
  int tabREMOVE[] = {'R', 'E', 'M', 'O', 'V', 'E'};
  int tabVALID[] = {'V', 'A', 'L', 'I', 'D'};
  int tabENERGY[] = {'E', 'N', 'E', 'R', 'G', 'Y'};
  int tabEQUAL[] = {'E', 'Q', 'U', 'A', 'L'};

  if (tempTab == NULL) { //blad w mallocu
    exit(1);
  }

  for (counter = 0; counter < 7 && tempChar != ' ' && tempChar != '\n' &&
    isEOF() == 0; counter++) {
    *(tempTab + counter) = tempChar;
    tempChar = getchar();
  }

  ungetc(tempChar, stdin);

  if (counter < 5) {
    free(tempTab);

    return -1;
  } else if (counter == 7 && compareStrings(tempTab, tabDECLARE, 7)) {
    free(tempTab);

    return 1;
  } else if (counter == 6 && compareStrings(tempTab, tabREMOVE, 6)) {
    free(tempTab);

    return 2;
  } else if (counter == 5 && compareStrings(tempTab, tabVALID, 5)) {
    free(tempTab);

    return 3;
  } else if (counter == 6 && compareStrings(tempTab, tabENERGY, 6)) {
    free(tempTab);

    return 4;
  } else if (counter == 5 && compareStrings(tempTab, tabEQUAL, 5)) {
    free(tempTab);

    return 5;
  } else {
    free(tempTab);

    return -1;
  }
} //getTypeOfOperation

//zwraca liczbe z wejscia lub [0] jesli blad
uint64_t getNumber() {
  uint64_t resultInt = 0;
  int actualChar;

  for (actualChar = getchar(); actualChar != '\n' &&
    actualChar != ' ' && isEOF() == 0; actualChar = getchar()) {
    if (isdigit(actualChar)) {
      uint64_t tempInt = getDigit(actualChar);

      if (((UINT64_MAX - tempInt) / 10) < resultInt) {
        return 0;
      }

      resultInt = resultInt * 10 + tempInt;
    } else {
      return 0;
    }
  }

  ungetc(actualChar, stdin);

  return resultInt;
} //getNumber

//zwraca [0] jesli wszystkie elementy sa dobre, [-1] jesli jest jakis niedozowlony znak
int checkLine() {
  char actualChar;
  int result = 0;

  for (actualChar = getchar(); actualChar != ' '
    && actualChar != '\n' && isEOF() == 0; actualChar = getchar()) {
    if (actualChar != '0' && actualChar != '1'
      && actualChar != '2' && actualChar != '3') {
      result = -1;
    }
  }

  ungetc(actualChar, stdin);

  return result;
} //checkLine

//wypisuje "ERROR" na wyjscie diagnostyczne
void printError() {
  skipOneLine();
  fprintf(stderr, "ERROR\n");
} //printError

//wypisuje "OK" na standardowe wyjscie lub "ERROR"
void outputAnswer(int argumentInt) {
  if (isEndOfLine() == 0 || isEOF()) {
    printError();
  } else if (argumentInt == 0) {
    fprintf(stdout, "OK\n");
  } else {
    printError();
  }
} //outputAnswer

//wypisuje liczbe na standardowe wyjscie lub "ERROR"
void outputNumber(uint64_t argumentInt) {
  if (isEndOfLine() == 0 || isEOF()) {
    printError();
  } else if (argumentInt == 0) {
    printError();
  } else {
    fprintf(stdout, "%"
    PRIu64
    "\n", argumentInt);
  }
} //outputNumber

//sprawdza czy nastepny znak jest znakiem konca lini: [1] - tak, [0] - nie
int isEndOfLine() {
  char actualChar = getchar();
  ungetc(actualChar, stdin);

  if (actualChar == '\n') {
    return 1;
  } else {
    return 0;
  }
} //isEndOfLine

//wypisuje na standardowe wyjscie "YES"/"NO" lub "ERROR"
void outputAnswerYN(int argumentInt) {
  if (isEndOfLine() == 0 || isEOF()) {
    printError();
  } else if (argumentInt == 1) {
    fprintf(stdout, "YES\n");
  } else if (argumentInt == 0) {
    fprintf(stdout, "NO\n");
  } else {
    printError();
  }
} //outputAnswerYN

//sprawdza czy nastepny znak jest nowa linia/spacja/czyms innym:
//[0] - spacja, [1] - nowa linia, [-1] - cos innego
int nextChar() {
  char actualChar = getchar();
  ungetc(actualChar, stdin);

  if (actualChar == ' ') {
    return 0;
  } else if (actualChar == '\n') {
    return 1;
  } else {
    return -1;
  }
} //nextChar

//pominia jeden znak z wejscia
void skipOneChar() {
  getchar();
} //skipOneChar

//zwraca nastepny znak jako cyfre, jesli nie jest cyfra to [10]
uint64_t getDigit(char actualChar) {
  if (actualChar == '0') {
    return 0;
  } else if (actualChar == '1') {
    return 1;
  } else if (actualChar == '2') {
    return 2;
  } else if (actualChar == '3') {
    return 3;
  } else if (actualChar == '4') {
    return 4;
  } else if (actualChar == '5') {
    return 5;
  } else if (actualChar == '6') {
    return 6;
  } else if (actualChar == '7') {
    return 7;
  } else if (actualChar == '8') {
    return 8;
  } else if (actualChar == '9') {
    return 9;
  } else {
    return 10;
  }
} //getDigit

//sprawdza czy nastepny znak jest cyfra
int checkNextCharForNumber() {
  char actualChar = getchar();
  ungetc(actualChar, stdin);

  if (isdigit(actualChar)) {
    return 1;
  } else {
    return 0;
  }
} //checkNextCharForNumber

//sprawdza czy zostal wczytany znak EOF
int isEOF() {
  return feof(stdin);
} //isEOF

