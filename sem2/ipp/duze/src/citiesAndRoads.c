/** @file
 * Implementacja klasy reprezentujacej miasta i drogi je laczace
 */
#include <stdlib.h>
#include <stdbool.h>
#include <inttypes.h>
#include "bitset.h"
#include "randomIdGenerator.h"
#include "citiesAndRoads.h"
#include "string.h"

/** @brief Struktura przechowujaca informacje o miescie.
 */
typedef struct CityInfo {
    uint64_t distance; ///< odleglosc od zrodla
    bool isInQ; ///< @p true jesli jest w kolejce, @p false jesli nie
    int year; ///< najstarszy odcinek drogi na sciezce
    City *prevCity; ///< wskaznik na poprzednie miasto w sciezce
    bool isGood; ///< @p true, jesli sciezka jest jednoznaczna, @p false wpp
} CityInfo;

/** @brief Struktura drzewa BST.
 */
typedef struct BST {
    unsigned cityId; ///< id miasta
    City *cityPointer; ///< wskaznik na miasto
    Road *roadPointer; ///< wskaznik na droge
    bool isRight; ///< @p true, jesli to prawe miasto drogi, @p false wpp
    struct BST *left; ///< wskazni na lewe poddrzewo
    struct BST *right; ///< wskaznik na prawe poddrzewa
} BST;

/** @brief Struktura reprezentujaca miasto.
 * Kazda miasto ma nazwe, id,
 * strukture trzymajaca sasiednie miasta,
 * strukture trzymajaca informacje o nim,
 * bitset trzymajacay drogi krajowe do ktorych nalezy
 */
struct City {
    unsigned id; ///< id miasta
    char *name; ///< nazwa miasta
    BST *neighbours; ///< sasiedzi miasta
    CityInfo *info; ///< wskaznik na info o miescie
    Bitset *routes; ///< bitset z drogami krajowymi
};

/** @brief Struktura reprezentujaca droge.
 * Kazda droga ma dlugosc, rok budowy/naprawy,
 * i miasta, ktore laczy
 */
struct Road {
    unsigned length; ///< dlugosc drogi
    int buildRepairYear; ///< rok budowy/remontu drogi
    City *city1; ///< wskanik na lewe miasto ktore laczy droga
    City *city2; ///< wskanik na prawe miasto ktore laczy droga
    Bitset *routes; ///< bitset z drogami krajowymi
};

/** @brief Struktura elementu kolejki
 */
typedef struct ElementQueue {
    struct ElementQueue *prevElem; ///< poprzedni element
    struct ElementQueue *nextElem; ///< nastepny element
    City *pointerCity; ///< wskaznik na miasto
} ElementQueue;

/** @brief Struktura kolejki
 */
struct Queue {
    ElementQueue *begElem; ///< poczatek kolejki
    ElementQueue *endElem; ///< koniec kolejki
};


Queue *createQueue() {
    Queue *resQueue = malloc(sizeof(Queue));
    if (resQueue == NULL) {
        return NULL;
    }

    resQueue->begElem = NULL;
    resQueue->endElem = NULL;

    return resQueue;
} //createQueue


void addQueue(Queue **argQueue, City *argCity) {
    if (*argQueue == NULL) {
        return;
    }

    if ((*argQueue)->begElem == NULL || (*argQueue)->endElem == NULL) {
        ElementQueue *tempElem = malloc(sizeof(ElementQueue));
        if (tempElem == NULL) {
            return;
        }

        tempElem->pointerCity = argCity;
        tempElem->nextElem = NULL;
        tempElem->prevElem = NULL;

        (*argQueue)->begElem = tempElem;
        (*argQueue)->endElem = tempElem;
    }
    else {
        ElementQueue *tempElem = malloc(sizeof(ElementQueue));
        if (tempElem == NULL) {
            return;
        }

        tempElem->pointerCity = argCity;
        tempElem->nextElem = NULL;
        tempElem->prevElem = (*argQueue)->endElem;
        ((*argQueue)->endElem)->nextElem = tempElem;
        (*argQueue)->endElem = tempElem;
    }
} //addQueue


City *frontQueue(Queue **argQueue) {
    if (*argQueue == NULL) {
        return NULL;
    }

    ElementQueue *tempElem = (*argQueue)->begElem;
    if (tempElem == NULL) {
        (*argQueue)->endElem = NULL;
        return NULL;
    }

    (*argQueue)->begElem = (*argQueue)->begElem->nextElem;
    City *resCity = tempElem->pointerCity;

    free(tempElem);

    return resCity;
} //frontQueue


bool isEmptyQueue(Queue *argQueue) {
    if (argQueue == NULL) {
        return false;
    }

    return (argQueue->begElem == NULL || argQueue->endElem == NULL);
} //isEmptyQueue


void deleteQueue(Queue **argQueue) {
    ElementQueue *tempElem = (*argQueue)->begElem;

    while (tempElem) {
        tempElem = tempElem->nextElem;
        free(tempElem->prevElem);
    }

    free(*argQueue);
} //deleteQueue


/** @brief Funkcja tworzaca strukture info o miescie.
 * @return Wskaznik na strukture, NULL jesli sie nie udalo
 */
CityInfo *createCityInfo() {
    CityInfo *resCityInfo = malloc(sizeof(CityInfo));
    if (resCityInfo == NULL) {
        return NULL;
    }

    resCityInfo->distance = UINT64_MAX;
    resCityInfo->prevCity = NULL;
    resCityInfo->year = INT32_MAX;
    resCityInfo->isGood = true;
    resCityInfo->isInQ = false;

    return resCityInfo;
} //createCityInfo


void resetCityInfo(City *argCity) {
    if (argCity == NULL) {
        return;
    }

    argCity->info->distance = UINT64_MAX;
    argCity->info->prevCity = NULL;
    argCity->info->year = INT32_MAX;
    argCity->info->isGood = true;
    argCity->info->isInQ = false;
} //resetCityInfo


void setCityDistance(City *argCity, uint64_t argDist) {
    if (argCity == NULL) {
        return;
    }

    argCity->info->distance = argDist;
} //setCityDistance


uint64_t getCityDistance(City *argCity) {
    if (argCity == NULL) {
        return UINT64_MAX;
    }

    return argCity->info->distance;
} //getCityDistance


int getCityYear(City *argCity) {
    if (argCity == NULL) {
        return UINT32_MAX;
    }

    return  argCity->info->year;
} //getCityYear

void setCityQ(City *argCity, bool argB) {
    if (argCity == NULL) {
        return;
    }

    argCity->info->isInQ = argB;
} //setCityQ


/** @brief Funkcja zwraca parametr przynaleznosci miasta do kolejki.
 * @param[in] argCity - dane miasto
 * @return @p true, jesli jest w kolejce, @p false wpp
 */
bool getCityQ(City *argCity) {
    if (argCity == NULL) {
        return false;
    }

    return argCity->info->isInQ;
} //getCityQ


void setCityPrev(City *argCity, City *argCityPrev) {
    if (argCity == NULL) {
        return;
    }

    argCity->info->prevCity = argCityPrev;
} //setCityPrev


City *getCityPrev(City *argCity) {
    if (argCity == NULL) {
        return NULL;
    }

    return argCity->info->prevCity;
} //getCityPrev


/** @brief Funkcja ustawia parametr jednoznacznosci sciezki
 * @param[in,out] argCity - dane miasto
 * @param[in] argB - ustawiany parametr
 */
void setIsGoodCity(City *argCity, bool argB) {
    if (argCity == NULL) {
        return;
    }

    argCity->info->isGood = argB;
} //setIsGoodCity


bool isGoodCity(City *argCity) {
    if (argCity == NULL) {
        return false;
    }

    return argCity->info->isGood;
} //isGoodCity


/** @brief Funkcja tworzy nowy element drzewo BST;
 * Funkcja alokuje pamiec i zwraca wskaznik na strukture
 * inicjalizowana miastem i droga,
 * NULL jesli alokacja sie nie udala.
 * @param[in] argCity - dane miasto
 * @param[in] argRoad - dana droga
 * @param[in] argIsRight - czy jest prawym koncem drogi
 * @return Wskaznik na drzewo BST, NULL jesli sie alokacja nie udala
 */
BST *createBST(City *argCity, Road *argRoad, bool argIsRight) {
    if (argCity == NULL) {
        return  NULL;
    }

    BST *resBST = malloc(sizeof(BST));
    if (resBST == NULL) {
        return NULL;
    }

    resBST ->left = NULL;
    resBST->right = NULL;
    resBST->roadPointer = argRoad;
    resBST->cityPointer = argCity;
    resBST->isRight = argIsRight;
    resBST->cityId = argCity->id;

    return resBST;
} //createBST


/** @brief Funkcja znajduje miasto w drzewie.
 * Funkcja znajduje miasto w dzrzewie i zwraca
 * wskaznik na droge laczaca miasta,
 * NULL jesli nie sa polaczone
 * @param[in] rootBST - przeszukiwane drzewo
 * @param[in] argCity - szukane miasto
 * @return Wskaznik na droge laczaca miasta, NULL jesli nie istnieje droga
 */
Road *findBST(BST *rootBST, City *argCity) {
    if (rootBST == NULL || argCity == NULL) {
        return NULL;
    }

    if (rootBST->cityPointer == argCity) {
        return rootBST->roadPointer;
    }

    if (argCity->id < rootBST->cityId) {
        return findBST(rootBST->left, argCity);
    }
    else {
        return findBST(rootBST->right, argCity);
    }
} //findBST


/** @brief Funkcja dodaje do drzewa miasto.
 * Funkcja dodaje do drzewa miasto i droge laczaca.
 * @param[in,out] rootBST - edytowane drzewo
 * @param[in] argCity - dodawane miasto
 * @param[in] argRoad - dodawana droga
 * @param[in] argIsRight - czy jest prawym koncem drogi
 */
void addBST(BST **rootBST, City *argCity, Road *argRoad, bool argIsRight) {
    if (argCity == NULL || argRoad == NULL) {
        return;
    }

    if (*rootBST == NULL) {
        *rootBST = createBST(argCity, argRoad, argIsRight);
    }
    else if (argCity->id < (*rootBST)->cityId) {
        addBST(&((*rootBST)->left), argCity, argRoad, argIsRight);
    }
    else {
        addBST(&((*rootBST)->right), argCity, argRoad, argIsRight);
    }
} //addBST


/** @brief Funkcja zwracajaca minimalny element w drzewie.
 * Funkcja zwraca wskaznik na namnijejszy elemen w drzewie,
 * NULL jesli sie nie udalo.
 * @param[in] rootBST - przeszukiwane drzewo
 * @return Wskaznik na najmniejszy elemen, NULL jesli sie nie udalo
 */
BST *removeMin(BST **rootBST) {
    if (*rootBST == NULL) {
        return NULL;
    }

    if ((*rootBST)->left == NULL) {
        BST *resBST = (*rootBST);
        (*rootBST) = (*rootBST)->right;

        return resBST;
    }
    else {
        return removeMin(&((*rootBST)->left));
    }
} //removeMin


/** @brief Funkcja usuwa z drzewa wartosc minimalna.
 * @param[in,out] rootBST - edytowane drzewo
 * @param[in] argCity - usuwane miasto
 */
void removeBST(BST **rootBST, City *argCity) {
    if (*rootBST == NULL || argCity == NULL) {
        return;
    }

    if ((*rootBST)->cityPointer == argCity) {
        if ((*rootBST)->right == NULL) {
            BST *tempRoot = (*rootBST);
            (*rootBST) = (*rootBST)->left;
            free(tempRoot);
        }
        else {
            BST *tempRoot = removeMin(&((*rootBST)->right));
            (*rootBST)->cityId = tempRoot->cityId;
            (*rootBST)->cityPointer = tempRoot->cityPointer;
            (*rootBST)->roadPointer = tempRoot->roadPointer;
            (*rootBST)->isRight = tempRoot->isRight;

            free(tempRoot);
        }
    }
    else if (argCity->id < (*rootBST)->cityId) {
        removeBST(&((*rootBST)->left), argCity);
    }
    else {
        removeBST(&((*rootBST)->right), argCity);
    }
} //removeBST


/** @brief Funkcja usuwa drzewo z pamiec.
 * Funkcja usuwa z pamieci cala strukture i
 * zwierajace drogi.
 */
void deleteBST(BST **rootBST) {
    if (*rootBST != NULL) {
        deleteBST(&((*rootBST)->left));
        deleteBST(&((*rootBST)->right));

        (*rootBST)->left = NULL;
        (*rootBST)->right = NULL;

        if ((*rootBST)->isRight) {
            deleteRoad(&((*rootBST)->roadPointer));
        }
        else {
            (*rootBST)->roadPointer = NULL;
        }

        free(*rootBST);
        (*rootBST) = NULL;
    }
} //deleteBST

/** @brief Funkcja zwraca mniejsza wartosc.
 * @param[in] argA - wartosc nr 1.
 * @param[in] argB - wartosc nr 2.
 * @return min{argA, argB}
 */
int min(int argA, int argB) {
    if (argA < argB) {
        return argA;
    }

    return argB;
} //min


/** @brief Funkcja ustawiajce rok dla miasta.
 * @param[in, out] argCity - edytowane miasto
 * @param[in] year - ustawiany rok
 */
void setCityYear2(City *argCity, int year) {
    if (argCity == NULL) {
        return;
    }

    argCity->info->year = year;
} //setCityYear2


/** Funkcja przechodzaca po sasiadach miasta i dodajace do kolejki zgodnie z SPFA.
 * @param[in] rootBST - przeszukiwane drzewo
 * @param[in] argCityPrev - poprzednie miasto
 * @param[in] argCityMeta1 - miasto koncowe
 * @param[in,out] argQ - edytowana kolejka
 * @param[in] routeId - id akutalizowanej drogi krajowej
 */
void allElementsBST(BST *rootBST, City *argCityPrev,
        City *argCityMeta1, Queue *argQ, unsigned routeId) {
    if (rootBST == NULL || argQ == NULL) {
        return;
    }

    City *tempCity = rootBST->cityPointer;
    Road *tempRoad = rootBST->roadPointer;

    if ((tempCity == argCityMeta1 || findBitset(tempCity->routes, routeId) == false) &&
        findBitset(tempRoad->routes, routeId) == false) {
        if (getCityDistance(tempCity) > getCityDistance(argCityPrev) + tempRoad->length) {
            setCityDistance(tempCity, getCityDistance(argCityPrev) + tempRoad->length);
            setCityPrev(tempCity, argCityPrev);

            setCityYear2(tempCity, min(getCityYear(argCityPrev), tempRoad->buildRepairYear));

            setIsGoodCity(tempCity, isGoodCity(argCityPrev));

            if (getCityQ(tempCity) == false) {
                addQueue(&argQ, tempCity);
            }

            setCityQ(tempCity, true);
        }
        else if (getCityDistance(tempCity) == getCityDistance(argCityPrev) + tempRoad->length) {
            if (getCityYear(tempCity) == min(getCityYear(argCityPrev), tempRoad->buildRepairYear)) {
                setIsGoodCity(tempCity, false);

                if (getCityQ(tempCity) == false) {
                    addQueue(&argQ, tempCity);
                }

                setCityQ(tempCity, true);
            }
            else if (getCityYear(tempCity) < min(getCityYear(argCityPrev), tempRoad->buildRepairYear)) {
                setIsGoodCity(tempCity, isGoodCity(argCityPrev));
                setCityYear2(tempCity, min(getCityYear(argCityPrev), tempRoad->buildRepairYear));
                setCityPrev(tempCity, argCityPrev);

                if (getCityQ(tempCity) == false) {
                    addQueue(&argQ, tempCity);
                }

                setCityQ(tempCity, true);
            }
        }
    }

    allElementsBST(rootBST->left, argCityPrev, argCityMeta1, argQ, routeId);
    allElementsBST(rootBST->right, argCityPrev, argCityMeta1, argQ, routeId);
} //allElementsBST


void allNeighbours(City *argCity, City *argCityMeta1, Queue *argQ, unsigned routeId) {
    if (argCity == NULL || argQ == NULL) {
        return;
    }

    allElementsBST(argCity->neighbours, argCity, argCityMeta1, argQ, routeId);
} //allNeighbours


City *createCity(const char *argName, RandomIdGen *argGen) {
    if (argName == NULL || argGen == NULL) {
        return NULL;
    }

    City *resCity = malloc(sizeof(City));
    if (resCity == NULL) {
        return NULL;
    }

    unsigned nameLength = stringLength(argName);
    char *resString = malloc(sizeof(char) * (nameLength + 1));

    for (unsigned i = 0; i < nameLength; i++) {
        resString[i] = argName[i];
    }

    resString[nameLength] = '\0';

    resCity->name = resString;
    resCity->id = newId(argGen);
    resCity->neighbours = NULL;
    resCity->routes = createBitset(1000);
    resCity->info = createCityInfo();

    return resCity;
} //createCity


Road *createRoad(unsigned argLength, City *argCity1, City *argCity2, int argYear) {
    if (argCity1 == NULL || argCity2 == NULL) {
        return NULL;
    }

    Road *resRoad = malloc(sizeof(Road));
    if (resRoad == NULL) {
        return NULL;
    }

    resRoad->length = argLength;
    resRoad->buildRepairYear = argYear;
    resRoad->city1 = argCity1;
    resRoad->city2 = argCity2;
    resRoad->routes = createBitset(1000);

    return resRoad;
} //createRoad


void deleteCity(City **argCity) {
    if (*argCity == NULL) {
        return;
    }

    deleteBST(&((*argCity)->neighbours));
    deleteBitset(&((*argCity)->routes));

    free((*argCity)->name);
	free((*argCity)->info);
    free(*argCity);
    (*argCity) = NULL;
} //deleteCity


void deleteRoad(Road **argRoad) {
    if (*argRoad == NULL) {
        return;
    }

    deleteBitset(&((*argRoad)->routes));
    free(*argRoad);
    (*argRoad) = NULL;
} //deleteRoad


const char *getCityName(City *argCity) {
    if (argCity == NULL) {
        return NULL;
    }

    return argCity->name;
} //getCityName


Road *findRoad(City *argCity1, City *argCity2) {
    if (argCity1 == NULL || argCity2 == NULL) {
        return NULL;
    }

    return findBST(argCity1->neighbours, argCity2);
} //findRoad


bool addRoadCity(City *argCity1, City *argCity2, unsigned length, int year) {
    if (argCity1 == NULL || argCity2 == NULL || length == 0) {
        return false;
    }

    Road *tempRoad = createRoad(length, argCity1, argCity2, year);
    if (tempRoad == NULL) {
        return false;
    }

    addBST(&(argCity1->neighbours), argCity2, tempRoad, false);
    addBST(&(argCity2->neighbours), argCity1, tempRoad, true);

    return true;
} //addRoadCity


bool addRoadCityElem(City *argCity1, City *argCity2, Road *tempRoad) {
    if (argCity1 == NULL || argCity2 == NULL || tempRoad == NULL) {
        return false;
    }

    addBST(&(argCity1->neighbours), argCity2, tempRoad, false);
    addBST(&(argCity2->neighbours), argCity1, tempRoad, true);

    return true;
} //addRoadCityElem


bool setRoadYear(Road *argRoad, int argYear) {
    if (argRoad == NULL || argYear == 0) {
        return false;
    }

    if (argRoad->buildRepairYear > argYear) {
        return false;
    }

    argRoad->buildRepairYear = argYear;

    return true;
} //setRoadYear


void setRouteCity(City *argCity, unsigned routeId) {
    if (argCity == NULL) {
        return;
    }

    addBitset(argCity->routes, routeId);
} //setRouteCity


void removeRoadCity(City *argCity1, City *argCity2) {
    removeBST(&(argCity1->neighbours), argCity2);
    removeBST(&(argCity2->neighbours), argCity1);
} //removeRoadCity


bool isInRouteCity(City *argCity, unsigned routeId) {
    if (argCity == NULL) {
        return false;
    }

    return findBitset(argCity->routes, routeId);
} //isInRouteCity


/** @brief Funkcja znajdujaca nastepne miasto w drodze krajowej.
 * Funkcja znajduje nastepne miasto w drodze krajowej,
 * inne niz poprzednik.
 * @param[in] rootBST - przeszukiwane drzewo
 * @param[in] argPrevCity - poprzednie miasto
 * @param[in] routeId - akualizowana droga krajowa
 * @return Wskaznik na nastepne miasto, NULL jesli nie istnieje
 */
City *getNextCityBST(BST *rootBST, City *argPrevCity, unsigned routeId) {
    if (rootBST == NULL) {
        return NULL;
    }

    if (findBitset(rootBST->roadPointer->routes, routeId) &&
        rootBST->cityPointer != argPrevCity) {
        return rootBST->cityPointer;
    }
    else {
        City *tempCity = getNextCityBST(rootBST->left, argPrevCity, routeId);

        if (tempCity == NULL) {
            return getNextCityBST(rootBST->right, argPrevCity, routeId);
        }

        return tempCity;
    }
} //getNextCityBST


City *getNextInRoute(City *argCity, City *argPrevCity, unsigned routeId) {
    if (argCity == NULL) {
        return NULL;
    }

    return getNextCityBST(argCity->neighbours, argPrevCity, routeId);
} //etNextInRoute


int getRoadYear(Road *argRoad) {
    if (argRoad == NULL) {
        return 0;
    }

    return argRoad->buildRepairYear;
} //getRoadYear


unsigned getRoadLength(Road *argRoad) {
    if (argRoad == NULL) {
        return 0;
    }

    return argRoad->length;
} //getRoadLength


void setRoadRoute(Road *argRoad, unsigned routeId) {
    if (argRoad == NULL) {
        return;
    }

    addBitset(argRoad->routes, routeId);
} //setRoadRoute


bool getRoadRoute(Road *argRoad, unsigned routeId) {
    if (argRoad == NULL){
        return false;
    }

    return findBitset(argRoad->routes, routeId);
} //getRoadRoute


void removeRoadRoute(Road *argRoad, unsigned routeId) {
    if (argRoad == NULL || routeId <= 0 || routeId > 999) {
        return;
    }

    removeBitset(argRoad->routes, routeId);
} //removeRoadRoute


void removeCityRoute(City *argCity, unsigned routeId) {
    if (argCity == NULL || routeId <= 0 || routeId > 999) {
        return;
    }

    removeBitset(argCity->routes, routeId);
} //removeCityRoute