#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include "iolib.h"
#include "listlib.h"
#include "dsulib.h"
#include "trielib.h"

//deklaruje ciaga z wejscia [0] - ok, [-1] - error
int declareTree(PointerOnTree *argumentPointerOnTree) {
  return addToTree(argumentPointerOnTree);
} //declareTree

//sprawdza czy w drzewie jest dany ciag [1] - istnieje, [0] - nie istnieje, [-1] - error
int validTree(PointerOnTree argumentPointerOnTree) {
  return findInTree(argumentPointerOnTree);
} //validTree

//usuwa z drzewa ciagi z danym prefiksem
int removeTree(PointerOnTree *argumentPointerOnTree) {
  return removeFromTree(argumentPointerOnTree);
} //removeTree

//dodaje/sprawdza energie zgodnie z wejsciem
void energyTree(PointerOnTree *argumentPointerOnTree) {
  ElementOfList *tempElementOfList = getSetFromTree(*argumentPointerOnTree);

  if (tempElementOfList == NULL) {
    printError();
  } else if (isEndOfLine()) { //koniec lini wiecz sprawdzamy energie
    outputNumber(getValueOfSet(*tempElementOfList));
  } else if (nextChar() == 0) { //spacja wiec edytujemy energie
    skipOneChar();

    if (checkNextCharForNumber() == 0) {
      printError();
    } else {
      uint64_t tempInt = getNumber();

      if (tempInt == 0) {
        printError();
      } else {
        outputAnswer(setValueOfSet(tempElementOfList, tempInt));
      }
    }
  } else {
    printError();
  }
} //energyTree

//tworzy relacje rownowaznosci miedzy dwoma ciagami
int equalTree(PointerOnTree *argumentPointerOnTree) {
  ElementOfList *tempElementOfList1 = getSetFromTree(*argumentPointerOnTree);

  if (isEndOfLine()) {
    return -1;
  } else if (nextChar() == 0) {
    skipOneChar();

    if (checkNextCharForNumber() == 0) {
      return -1;
    }

    ElementOfList *tempElementOfList2 = getSetFromTree(*argumentPointerOnTree);

    if ((tempElementOfList1 != NULL && tempElementOfList2 != NULL)) {
      return unionSets(tempElementOfList1, tempElementOfList2);
    } else {
      return -1;
    }
  } else {
    return -1;
  }
} //equalTree

int main() {
  PointerOnTree tree = createTree(); //tworzymy drzewo

  if (tree == NULL) { //blad w mallocu
    exit(1);
  }

  for (int actualTypeOfOperation = getTypeOfOperation();
    actualTypeOfOperation != 6; actualTypeOfOperation = getTypeOfOperation()) {
    int actualChar = nextChar();

    if (actualChar != 0 || actualTypeOfOperation ==
      -1) { //poprawnosc wejscia: po nazwie operacji musi byc spacja
      printError();
    } else {
      skipOneChar();

      if (checkNextCharForNumber()) {
        if (actualTypeOfOperation == 1) { //DECLARE
          outputAnswer(declareTree(&tree));
        } else if (actualTypeOfOperation == 2) { //REMOVE
          outputAnswer(removeTree(&tree));
        } else if (actualTypeOfOperation == 3) { //VALID
          outputAnswerYN(validTree(tree));
        } else if (actualTypeOfOperation == 4) { //ENERGY
          energyTree(&tree);
        } else if (actualTypeOfOperation == 5) { //EQUAL
          outputAnswer(equalTree(&tree));
        }
      } else {
        printError();
      }
    }
  }

  deleteTree(&tree);

  return 0;
} //main

