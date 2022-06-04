//
// Created by Marcin Abramowicz and Jakub Organa
//

#include <string>
#include <unordered_map>
#include <tuple>
#include <queue>
#include <iostream>
#include <cstddef>

#include "poset.h"

namespace {

#ifdef NDEBUG
  const bool debug = false;
#else
  const bool debug = true;
#endif

// ====================== TYPES ================================================

  using string = std::string;
  using boolTriple = std::tuple<bool, bool, bool>;

  using mapOfStringPointersAndBool = std::unordered_map<const string *, bool>;
  using adjacencyMap = mapOfStringPointersAndBool;

  using tripleOfIntAndMapsOfStringPointersAndBool =
  std::tuple<mapOfStringPointersAndBool, mapOfStringPointersAndBool, int>;

  using mapOfTriplesOfIntAndMapsOfStringPointersAndBool =
  std::unordered_map<string, tripleOfIntAndMapsOfStringPointersAndBool>;
  using poset = mapOfTriplesOfIntAndMapsOfStringPointersAndBool;

  using mapOfMapsOfTriplesOfIntAndMapsOfStringPointersAndBool =
  std::unordered_map<unsigned long,
    mapOfTriplesOfIntAndMapsOfStringPointersAndBool>;
  using mapOfPosets = mapOfMapsOfTriplesOfIntAndMapsOfStringPointersAndBool;


// ====================== DECLARATIONS =========================================

  // Bfs colour counter.
  int bfsVisitColour = 0;

  // Static posetes getter.
  mapOfPosets &getPosets() {
    static mapOfPosets posets;
    return posets;
  }

  // Static empty poset getter.
  poset &getEmptyPoset() {
    static poset emptyPoset;
    return emptyPoset;
  }



// ====================== DEBUG ================================================

  // Function logs message on standard diagnostic stream.
  void log(const string &message) {
    static std::ios_base::Init init;

    if (debug) {
      std::cerr << message << std::endl;
    }
  }

  // Function wraps unsigned long in string.
  string wrapArg(unsigned long id) {
    return std::to_string(id);
  }

  // Functiom wraps value in "{value}".
  string wrapCorrectArg(char const *value1) {
    return "\"" + std::string(value1) + "\"";
  }

  // Function wraps argument in "{value}" or "NULL" if value is null.
  string wrapArg(char const *value1) {

    const static string NULL_ARG = "\"NULL\"";

    return value1 == nullptr ? NULL_ARG : wrapCorrectArg(value1);
  }

  // Function build prefix: ", relation ("{value1}, "{value2}")".
  string buildRelationPrefix(char const *value1, char const *value2) {

    const static string DELIMITER = ", ";

    return ", relation (" + wrapArg(value1)
      + DELIMITER + wrapArg(value2) + ")";
  }

// ---------------------- logging input ----------------------------------------

  // Function merges function name and arguments in brackets.
  string mergeNameAndArgumentsInput(const string &funcName,
    const string &funcArgs) {
    return funcName + "(" + funcArgs + ")";
  }

  // Function logs calling function name.
  void logInput(const string &funcName) {

    const static string funcArgs = "";

    log(mergeNameAndArgumentsInput(funcName, funcArgs));
  }

  // Function logs calling function name and poset id.
  void logInput(const string &funcName, const unsigned long id) {

    string funcArgs = wrapArg(id);

    log(mergeNameAndArgumentsInput(funcName, funcArgs));
  }

  // Function logs calling function name, poset id and value.
  void logInput(const string &funcName, const unsigned long id,
    char const *value) {

    const string DELIMITER = ", ";
    string funcArgs = wrapArg(id)
      + DELIMITER + wrapArg(value);

    log(mergeNameAndArgumentsInput(funcName, funcArgs));
  }

  // Function logs calling function name, poset id, value1 and value2.
  void logInput(const string &funcName, const unsigned long id,
    char const *value1, char const *value2) {

    const string DELIMITER = ", ";
    string funcArgs = wrapArg(id)
      + DELIMITER + wrapArg(value1)
      + DELIMITER + wrapArg(value2);

    log(mergeNameAndArgumentsInput(funcName, funcArgs));
  }

// ---------------------- assertions -------------------------------------------

  // Function returns true if value if null.
  bool isNull(char const *value) {
    return value == nullptr;
  }

  // Function returns message: "{funcName}: {message}".
  string buildFuncPrefix(const string &funcName, const string &message) {
    return funcName + ": " + message;
  }

  // Function returns message: "invelid {argName} ({arg})".
  string buildInvalidArgumentMessage(const string &argName, const string &arg) {
    return "invalid " + argName + " (" + arg + ")";
  }

  // Function checks value and logs if is invalid.
  bool assertInput(const string &funcName, char const *value) {

    const static string VALUE_NAME = "value";
    const static string NULL_ARG = "NULL";

    bool assertionResult = true;

    if (isNull(value)) {
      string message = buildFuncPrefix(funcName,
        buildInvalidArgumentMessage(VALUE_NAME, NULL_ARG));

      log(message);
      assertionResult = false;
    }

    return assertionResult;
  }

  // Function checks two values and logs if are invalid.
  bool assertInput(const string &funcName, char const *value1,
    char const *value2) {

    const static string VALUE1_NAME = "value1";
    const static string VALUE2_NAME = "value2";
    const static string NULL_ARG = "NULL";

    bool assertionResult = true;

    if (isNull(value1)) {
      string message = buildFuncPrefix(funcName,
        buildInvalidArgumentMessage(VALUE1_NAME, NULL_ARG));

      log(message);
      assertionResult = false;
    }

    if (isNull(value2)) {
      string message = buildFuncPrefix(funcName,
        buildInvalidArgumentMessage(VALUE2_NAME, NULL_ARG));

      log(message);
      assertionResult = false;
    }

    return assertionResult;
  }

// ---------------------- logging output ---------------------------------------

  // Function builds message prefix: "poset {id}".
  string buildPosetWithIdPrefix(const unsigned long id) {
    return "poset " + wrapArg(id);
  }

  // Function builds message: "{funcName}: {posetPrefix} {message}".
  string buildMessageWithFuncName(const string &funcName,
    const unsigned long id, const string &message) {

    return funcName + ": " + buildPosetWithIdPrefix(id) + message;
  }

  // Function builds message if element does not exist:
  // ", element {value1} or {value2} does not exist".
  string buildElementsDoesNotExistMessage(char const *value1,
    char const *value2) {

    return ", element " + wrapArg(value1) + " or "
      + wrapArg(value2) + " does not exist";
  }

  // Function chooses message if elements exist.
  string buildPosetMessageIfElementsExist(const string &message1,
    const string &message2, bool doElementsExist) {

    return doElementsExist ? message1 : message2;
  }

  // Function builds message if poset exist.
  string buildMessageIfPosetExist(const string &funcName,
    const unsigned long id, const string &message, bool doesPosetExist) {

    const static string DOES_NOT_EXIST_MESS = " does not exist";

    return doesPosetExist
      ? buildMessageWithFuncName(funcName, id, message)
      : buildMessageWithFuncName(funcName, id, DOES_NOT_EXIST_MESS);
  }

  // Function logs result of poset_new.
  void logPosetNew(const string &funcName, const unsigned long id) {

    const static string CREATED_MESS = " created";
    string message = buildMessageWithFuncName(funcName, id, CREATED_MESS);

    log(message);
  }

  // Function logs result of poset_delete.
  void logPosetDelete(const string &funcName, const unsigned long id,
    bool doesPosetExist) {

    const static string DELETED_MESS = " deleted";
    string message =
      buildMessageIfPosetExist(funcName, id, DELETED_MESS, doesPosetExist);

    log(message);
  }

  // Function builds poset_size message.
  string buildPosetSizeMessage(size_t posetSize) {
    return " contains " + std::to_string(posetSize) + " element(s)";
  }

  // Function logs result of poset_size.
  void logPosetSize(const string &funcName, const unsigned long id,
    size_t posetSize, bool doesPosetExist) {

    string posetSizeMessage = buildPosetSizeMessage(posetSize);
    string message =
      buildMessageIfPosetExist(funcName, id, posetSizeMessage, doesPosetExist);

    log(message);
  }

  // Function builds poset_insert message prefix.
  string buildPosetInsertPrefix(char const *value) {
    const static string ELEMENT_MESS = ", element ";

    return ELEMENT_MESS + wrapArg(value);
  }

  // Function builds poset_insert message.
  string buildPosetInsertMessage(char const *value, bool inserted) {

    const static string INSERTED_MESS = " inserted";
    const static string ALREADY_EXISTS_MESS = " already exists";

    return inserted
      ? buildPosetInsertPrefix(value) + INSERTED_MESS
      : buildPosetInsertPrefix(value) + ALREADY_EXISTS_MESS;
  }

  // Function logs result of poset_insert.
  void logPosetInsert(const string &funcName, const unsigned long id,
    char const *value,
    bool inserted, bool doesPosetExist) {

    string posetInsertMessage = buildPosetInsertMessage(value, inserted);
    string message = buildMessageIfPosetExist(funcName, id,
      posetInsertMessage, doesPosetExist);

    log(message);
  }

  // Function builds poset_remove message.
  string buildPosetRemoveMessage(char const *value, bool removed) {

    const static string REMOVED_MESS = " removed";
    const static string ALREADY_EXISTS_MESS = " does not exist";

    return removed
      ? buildPosetInsertPrefix(value) + REMOVED_MESS
      : buildPosetInsertPrefix(value) + ALREADY_EXISTS_MESS;
  }

  // Function logs result of poset_remove.
  void logPosetRemove(const string &funcName, const unsigned long id,
    char const *value,
    bool removed, bool doesPosetExist) {

    string posetRemoveMessage = buildPosetRemoveMessage(value, removed);
    string message =
      buildMessageIfPosetExist(funcName, id, posetRemoveMessage,
        doesPosetExist);

    log(message);
  }

  // Function builds poset_add message.
  string buildPosetAddMessage(char const *value1, char const *value2,
    bool doesRelationExist, bool doElementsExist) {

    const static string ADDED_MESS = " added";
    const static string CANNOT_BE_ADDED_MESS = " cannot be added";

    return doesRelationExist
      ? buildRelationPrefix(value1, value2) + ADDED_MESS
      : buildPosetMessageIfElementsExist(
        buildRelationPrefix(value1, value2) + CANNOT_BE_ADDED_MESS,
        buildElementsDoesNotExistMessage(value1, value2),
        doElementsExist);
  }

  // Function logs result of poset_add.
  void logPosetAdd(const string &funcName, const unsigned long id,
    char const *value1,
    char const *value2, bool doesRelationExist,
    bool doElementsExist,
    bool doesPosetExist) {

    const static string DOES_NOT_EXIST_MESS = " does not exist";
    string posetAddMessage =
      buildPosetAddMessage(value1, value2, doesRelationExist, doElementsExist);
    string message = doesPosetExist
      ? buildMessageWithFuncName(funcName, id, posetAddMessage)
      : buildMessageWithFuncName(funcName, id, DOES_NOT_EXIST_MESS);

    log(message);
  }

  // Function builds poset_del message.
  string buildPosetDelMessage(char const *value1, char const *value2,
    bool doesRelationExist, bool doElementsExist) {

    const static string DELETED_MESS = " deleted";
    const static string CANNOT_BE_DELETED_MESS = " cannot be deleted";

    return doesRelationExist
      ? buildRelationPrefix(value1, value2) + DELETED_MESS
      : buildPosetMessageIfElementsExist(
        buildRelationPrefix(value1, value2) + CANNOT_BE_DELETED_MESS,
        buildElementsDoesNotExistMessage(value1, value2), doElementsExist);
  }

  // Function logs result of poset_del.
  void logPosetDel(const string &funcName, const unsigned long id,
    char const *value1,
    char const *value2, bool doesRelationExist,
    bool doElementsExist,
    bool doesPosetExist) {

    string posetDelMessage =
      buildPosetDelMessage(value1, value2, doesRelationExist, doElementsExist);
    string message =
      buildMessageIfPosetExist(funcName, id, posetDelMessage, doesPosetExist);

    log(message);
  }

  // Function builds poset_test message.
  string buildPosetTestMessage(char const *value1, char const *value2,
    bool doesRelationExist, bool doElementsExist) {

    const static string EXISTS_MESS = " exists";
    const static string DOES_NOT_EXISTS_MESS = " does not exist";

    return doesRelationExist
      ? buildRelationPrefix(value1, value2) + EXISTS_MESS
      : buildPosetMessageIfElementsExist(
        buildRelationPrefix(value1, value2) + DOES_NOT_EXISTS_MESS,
        buildElementsDoesNotExistMessage(value1, value2),
        doElementsExist);
  }

  // Function logs result of poset_test.
  void logPosetTest(const string &funcName, const unsigned long id,
    char const *value1,
    char const *value2, bool doesRelationExist,
    bool doElementsExist,
    bool doesPosetExist) {

    string posetTestMessage =
      buildPosetTestMessage(value1, value2, doesRelationExist, doElementsExist);
    string message =
      buildMessageIfPosetExist(funcName, id, posetTestMessage, doesPosetExist);

    log(message);
  }

  // Function logs result of poset_clear.
  void logPosetClear(const string &funcName, const unsigned long id,
    bool doesPosetExist) {

    const static string CLEARED_MESS = " cleared";
    string message =
      buildMessageIfPosetExist(funcName, id, CLEARED_MESS, doesPosetExist);

    log(message);
  }

// ====================== USECASE ==============================================

  // boolTriple first element getter.
  bool getFirstBool(const boolTriple &boolTriple) {
    return std::get<0>(boolTriple);
  }

  // boolTriple second element getter.
  bool getSecondBool(const boolTriple &boolTriple) {
    return std::get<1>(boolTriple);
  }

  // boolTriple third element getter.
  bool getThirdBool(const boolTriple &boolTriple) {
    return std::get<2>(boolTriple);
  }

  // Function returns true if poset exist.
  bool posetExists(const mapOfPosets::iterator &posetIt) {
    return posetIt != getPosets().end();
  }

  // Function returns true if poset exist.
  bool posetExists(const unsigned long id) {
    return getPosets().count(id) > 0;
  }

  // Function returns true, if POSET with given id exists,
  // and contains records with given values.
  // Number of values is controlled by function overloading.
  boolTriple checkPosetAndFirstRecordExistance(
    mapOfPosets::iterator &thisPosetIterator, unsigned long id,
    const char *value1, poset::iterator &firstRecordIterator) {

    thisPosetIterator = getPosets().find(id);

    if (!posetExists(thisPosetIterator)) {
      return {false, false, false};
    }

    poset &thisPoset = thisPosetIterator->second;

    string value1Str = string(value1);
    firstRecordIterator = thisPoset.find(value1Str);

    if (firstRecordIterator == thisPoset.end()) {
      return {false, true, false};
    }

    return {true, true, true};
  }

  // Function checks record existence
  boolTriple checkPosetAndRecordsExistance(
    mapOfPosets::iterator &thisPosetIterator, unsigned long id,
    const char *value1, poset::iterator &firstRecordIterator,
    const char *value2, poset::iterator &secondRecordIterator) {

    boolTriple firstCheck = checkPosetAndFirstRecordExistance(thisPosetIterator,
      id, value1, firstRecordIterator);

    if (!std::get<0>(firstCheck)) {
      return firstCheck;
    }

    poset &thisPoset = thisPosetIterator->second;

    string value2Str = string(value2);
    secondRecordIterator = thisPoset.find(value2Str);

    if (secondRecordIterator == thisPoset.end()) {
      return {false, true, false};
    }

    return {true, true, true};
  }

  // Function checks records existence
  boolTriple checkPosetAndRecordsExistance(
    mapOfPosets::iterator &thisPosetIterator, unsigned long id,
    const char *value1, poset::iterator &firstRecordIterator) {

    return checkPosetAndFirstRecordExistance(thisPosetIterator, id, value1,
      firstRecordIterator);
  }


  // originalString is no loner a son of elements stored in it's 'fathers' map.
  // Every father of originalString becomes a father of every originalString's son.
  void delFromSonsOfFathers(poset &thisPoset, const string &originalString,
    adjacencyMap &fathers, adjacencyMap &sons) {

    for (auto &fatherPointerRecord : fathers) {
      const string *fatherPointer = fatherPointerRecord.first;
      auto fatherRecordIterator = thisPoset.find(*fatherPointer);
      adjacencyMap &fatherSons = std::get<1>(fatherRecordIterator->second);

      fatherSons.erase(&originalString);

      for (auto &sonPointerRecord : sons) {
        const string *sonPointer = sonPointerRecord.first;
        fatherSons[sonPointer] = true;
      }
    }
  }

  // originalString is no longer a father of elements stored in it's 'sons' map.
  // Every son of originalString becomes a son of every originalString's father.
  void delFromFathersOfSons(poset &thisPoset, const string &originalString,
    adjacencyMap &sons, adjacencyMap &fathers) {

    for (auto &sonPointerRecord : sons) {
      const string *sonPointer = sonPointerRecord.first;
      auto sonRecordIterator = thisPoset.find(*sonPointer);
      adjacencyMap &sonFathers = std::get<0>(sonRecordIterator->second);

      sonFathers.erase(&originalString);

      for (auto &fatherPointerRecord : fathers) {
        const string *fatherPointer = fatherPointerRecord.first;
        sonFathers[fatherPointer] = true;
      }
    }
  }

  // Function deletes all relations of an element in POSET.
  void delRelations(poset::iterator thisRecordIterator, poset &thisPoset) {
    const string &originalString = thisRecordIterator->first;
    adjacencyMap &fathers = std::get<0>(thisRecordIterator->second);
    adjacencyMap &sons = std::get<1>(thisRecordIterator->second);

    delFromSonsOfFathers(thisPoset, originalString, fathers, sons);
    delFromFathersOfSons(thisPoset, originalString, sons, fathers);
  }


  // Function checks if first element precedes second element in POSET.
  // If searchForLonger == true, algorithm searches for a path that is minimum
  // 2 edges long.
  bool areRelated(poset &thisPoset, poset::iterator firstRecordIterator,
    poset::iterator secondRecordIterator, bool searchForLonger) {

    bfsVisitColour++;

    std::queue<std::pair<poset::iterator, int> > recordsQueue;
    std::pair<poset::iterator, int> start = {firstRecordIterator, 0};

    recordsQueue.push(start);
    std::get<2>(firstRecordIterator->second) = bfsVisitColour;

    while (!recordsQueue.empty()) {
      auto front = recordsQueue.front();
      recordsQueue.pop();

      auto actIterator = front.first;
      int steps = front.second;

      adjacencyMap &sons = std::get<1>(actIterator->second);

      for (auto &sonPointerRecord : sons) {
        const string *sonPointer = sonPointerRecord.first;
        auto sonRecordIterator = thisPoset.find(*sonPointer);

        if (sonRecordIterator != secondRecordIterator) {
          int &visited = std::get<2>(sonRecordIterator->second);

          if (visited != bfsVisitColour) {
            visited = bfsVisitColour;
            recordsQueue.push({sonRecordIterator, steps + 1});
          }
        } else if (!searchForLonger or steps > 0) {
          return true;
        }
      }
    }

    return false;
  }

  // Element represented by orgString becomes physical father of all elements
  // stored in "secondSons".
  void switchFirstToSecondSons(poset &thisPoset, adjacencyMap &firstSons,
    adjacencyMap &secondSons, const string &orgString) {

    for (auto &secondSonPointerRecord : secondSons) {
      const string *secondSonPointer = secondSonPointerRecord.first;

      if (firstSons.find(secondSonPointer) == firstSons.end()) {
        auto secondSonRecordIterator = thisPoset.find(*secondSonPointer);
        adjacencyMap &secondSonFathers =
          std::get<0>(secondSonRecordIterator->second);

        firstSons[secondSonPointer] = true;
        secondSonFathers[&orgString] = true;
      }
    }
  }

  // Element represented by orgString becomes physical son of all elements
  // stored in "firstFathers".
  void switchFirstFathersToSecond(poset &thisPoset, adjacencyMap &firstFathers,
    adjacencyMap &secondFathers, const string &orgString) {

    for (auto &firstFatherPointerRecord : firstFathers) {
      const string *firstFatherPointer = firstFatherPointerRecord.first;

      if (secondFathers.find(firstFatherPointer) == secondFathers.end()) {
        auto firstFatherRecordIterator = thisPoset.find(*firstFatherPointer);
        adjacencyMap &firstFatherSons =
          std::get<1>(firstFatherRecordIterator->second);

        secondFathers[firstFatherPointer] = true;
        firstFatherSons[&orgString] = true;
      }
    }
  }

// ====================== IMPLEMENTATION =======================================


// ---------------------- posetNewImpl -----------------------------------------

  // Function finds minimal available poset id.
  unsigned long findMinimalPosetId() {
    unsigned long resultNumber = 0;

    for (; getPosets().count(resultNumber); resultNumber++) {
      //empty
    }

    return resultNumber;
  }

  // Implementation of
  // unsigned long poset_new().
  unsigned long posetNewImpl() {
    unsigned long newId = findMinimalPosetId();

    getPosets().insert({newId, getEmptyPoset()});

    return newId;
  }

// ---------------------- posetDeleteImpl --------------------------------------

  // Implementation of
  // void poset_delete(unsigned long id).
  bool posetDeleteImpl(unsigned long id) {

    if (posetExists(id)) {
      getPosets().erase(id);
      return true;
    }

    return false;
  }

// ---------------------- posetSizeImpl ----------------------------------------

  // Function returns poset with given id.
  poset getPosetWithId(unsigned long id) {
    return getPosets().find(id)->second;
  }

  // Implementation of
  // size_t poset_size(unsigned long id).
  std::pair<size_t, bool> posetSizeImpl(unsigned long id) {

    if (posetExists(id)) {
      return {getPosetWithId(id).size(), true};
    }

    return {0, false};
  }

// ---------------------- posetInsertImpl --------------------------------------

  // Implementation of
  // bool poset_insert(unsigned long id, char const *value).
  std::pair<bool, bool> posetInsertImpl(unsigned long id, char const *value) {

    auto thisPosetIterator = getPosets().find(id);

    if (!posetExists(thisPosetIterator)) {
      return {false, false};
    }

    poset &thisPoset = thisPosetIterator->second;
    string valueStr = string(value);

    if (thisPoset.find(valueStr) != thisPoset.end()) {
      return {false, true};
    }

    adjacencyMap fathers, sons;
    thisPoset[valueStr] = {fathers, sons, 0};

    return {true, true};
  }

// ---------------------- posetRemoveImpl --------------------------------------

  // Implementation of
  // bool poset_remove(unsigned long id, char const *value).
  std::pair<bool, bool> posetRemoveImpl(unsigned long id, char const *value) {
    mapOfPosets::iterator thisPosetIterator;
    poset::iterator thisRecordIterator;

    if (!std::get<0>(checkPosetAndRecordsExistance(thisPosetIterator, id, value,
      thisRecordIterator))) {
      return {false, posetExists(thisPosetIterator)};
    }

    poset &thisPoset = thisPosetIterator->second;

    delRelations(thisRecordIterator, thisPoset);

    thisPoset.erase(thisRecordIterator);
    return {true, true};
  }

// ---------------------- posetAddImpl -----------------------------------------

  // Implementation of
  // bool poset_add(unsigned long id, char const *value1, char const *value2).
  boolTriple posetAddImpl(unsigned long id, char const *value1,
    char const *value2) {

    mapOfPosets::iterator thisPosetIterator;
    poset::iterator firstRecordIterator, secondRecordIterator;

    boolTriple existCheckResult = checkPosetAndRecordsExistance(
      thisPosetIterator, id,
      value1, firstRecordIterator, value2, secondRecordIterator);

    if (!std::get<0>(existCheckResult)) {
      return existCheckResult;
    }

    poset &thisPoset = thisPosetIterator->second;

    // Check if strings are alredy related
    if (
      areRelated(thisPoset, firstRecordIterator, secondRecordIterator, false) or
        areRelated(thisPoset, secondRecordIterator, firstRecordIterator,
          false) or
        firstRecordIterator == secondRecordIterator) {
      return {false, true, true};
    }

    // Add string pointers to adjcacency maps
    adjacencyMap &firstSons = std::get<1>(firstRecordIterator->second);
    adjacencyMap &secondFathers = std::get<0>(secondRecordIterator->second);

    firstSons[&(secondRecordIterator->first)] = true;
    secondFathers[&(firstRecordIterator->first)] = true;

    return {true, true, true};
  }

// ---------------------- posetDelImpl -----------------------------------------

  // Implementation of
  // bool poset_del(unsigned long id, char const *value1, char const *value2).
  boolTriple posetDelImpl(unsigned long id, char const *value1,
    char const *value2) {

    mapOfPosets::iterator thisPosetIterator;
    poset::iterator firstRecordIterator, secondRecordIterator;

    boolTriple existCheckResult = checkPosetAndRecordsExistance(
      thisPosetIterator, id,
      value1, firstRecordIterator, value2, secondRecordIterator);

    if (!std::get<0>(existCheckResult)) {
      return existCheckResult;
    }

    poset &thisPoset = thisPosetIterator->second;

    // False, if strings are not related, or their relation is obligatory for
    // POSET to exist
    if (firstRecordIterator == secondRecordIterator or
      !areRelated(thisPoset, firstRecordIterator, secondRecordIterator,
        false) or
      areRelated(thisPoset, firstRecordIterator, secondRecordIterator,
        true)) {
      return {false, true, true};
    }

    adjacencyMap &firstSons = std::get<1>(firstRecordIterator->second);
    adjacencyMap &firstFathers = std::get<0>(firstRecordIterator->second);
    adjacencyMap &secondSons = std::get<1>(secondRecordIterator->second);
    adjacencyMap &secondFathers = std::get<0>(secondRecordIterator->second);

    const string &firstOrgString = firstRecordIterator->first;
    const string &secondOrgString = secondRecordIterator->first;

    // first string becomes a physical father of all sons of second string
    // second string becomes a physical son of all fathers of first string
    switchFirstToSecondSons(thisPoset, firstSons, secondSons, firstOrgString);
    switchFirstFathersToSecond(thisPoset, firstFathers, secondFathers,
      secondOrgString);

    // erase string pointers from adjacency maps
    firstSons.erase(&(secondRecordIterator->first));
    secondFathers.erase(&(firstRecordIterator->first));

    return {true, true, true};
  }

// ---------------------- posetTestImpl ----------------------------------------

  // Implementation of
  // bool poset_test(unsigned long id, char const *value1, char const *value2).
  boolTriple posetTestImpl(unsigned long id, char const *value1,
    char const *value2) {

    mapOfPosets::iterator thisPosetIterator;
    poset::iterator firstRecordIterator, secondRecordIterator;

    boolTriple existCheckResult = checkPosetAndRecordsExistance(
      thisPosetIterator, id,
      value1, firstRecordIterator, value2, secondRecordIterator);

    if (!std::get<0>(existCheckResult)) {
      return existCheckResult;
    }

    poset &thisPoset = thisPosetIterator->second;

    if (firstRecordIterator == secondRecordIterator) {
      return {true, true, true};
    }

    return {
      areRelated(thisPoset, firstRecordIterator, secondRecordIterator, false),
      true, true};
  }

// ---------------------- pposetClearImpl --------------------------------------

  // Implementation of
  // void poset_clear(unsigned long id).
  bool posetClearImpl(unsigned long id) {

    if (posetExists(id)) {
      getPosets().insert_or_assign(id, getEmptyPoset());
      return true;
    }

    return false;
  }

}
// ====================== FACADE ===============================================

namespace jnp1 {

  // Tworzy nowy poset i zwraca jego identyfikator.
  unsigned long poset_new() {
    const static string FUNC_NAME = "poset_new";
    logInput(FUNC_NAME);

    unsigned long newPosetId = posetNewImpl();
    logPosetNew(FUNC_NAME, newPosetId);

    return newPosetId;
  }

  // Jeżeli istnieje poset o identyfikatorze id, usuwa go, a w przeciwnym
  // przypadku nic nie robi.
  void poset_delete(unsigned long id) {

    const static string FUNC_NAME = "poset_delete";
    logInput(FUNC_NAME, id);

    bool isDeleted = posetDeleteImpl(id);
    logPosetDelete(FUNC_NAME, id, isDeleted);
  }

  // Jeżeli istnieje poset o identyfikatorze id, to wynikiem jest liczba jego
  // elementów, a w przeciwnym przypadku 0.
  size_t poset_size(unsigned long id) {

    const static string FUNC_NAME = "poset_size";
    logInput(FUNC_NAME, id);

    auto posetSizeRes = posetSizeImpl(id);
    logPosetSize(FUNC_NAME, id, posetSizeRes.first, posetSizeRes.second);

    return posetSizeRes.first;
  }


  // Jeżeli istnieje poset o identyfikatorze id i element value nie należy do
  // tego zbioru, to dodaje element do zbioru, a w przeciwnym przypadku nic nie
  // robi. Nowy element nie jest w relacji z żadnym elementem. Wynikiem jest
  // true, gdy element został dodany, a false w przeciwnym przypadku.
  bool poset_insert(unsigned long id, char const *value) {

    const static string FUNC_NAME = "poset_insert";
    logInput(FUNC_NAME, id, value);

    if (!assertInput(FUNC_NAME, value)) {
      return false;
    }

    auto posetInsertRes = posetInsertImpl(id, value);
    logPosetInsert(FUNC_NAME, id, value,
      posetInsertRes.first, posetInsertRes.second);

    return posetInsertRes.first;
  }

  // Jeżeli istnieje poset o identyfikatorze id i element value należy do tego
  // zbioru, to usuwa element ze zbioru oraz usuwa wszystkie relacje tego
  // elementu, a w przeciwnym przypadku nic nie robi. Wynikiem jest true, gdy
  // element został usunięty, a false w przeciwnym przypadku.
  bool poset_remove(unsigned long id, char const *value) {

    const static string FUNC_NAME = "poset_remove";
    logInput(FUNC_NAME, id, value);

    if (!assertInput(FUNC_NAME, value)) {
      return false;
    }

    auto posetRemoveRes = posetRemoveImpl(id, value);
    logPosetRemove(FUNC_NAME, id, value,
      posetRemoveRes.first, posetRemoveRes.second);

    return posetRemoveRes.first;
  }

  // Jeżeli istnieje poset o identyfikatorze id oraz elementy value1 i value2
  // należą do tego zbioru i nie są w relacji, to rozszerza relację w taki
  // sposób, aby element value1 poprzedzał element value2 (domyka relację
  // przechodnio), a w przeciwnym przypadku nic nie robi. Wynikiem jest true,
  // gdy relacja została rozszerzona, a false w przeciwnym przypadku.
  bool poset_add(unsigned long id, char const *value1, char const *value2) {

    const static string FUNC_NAME = "poset_add";
    logInput(FUNC_NAME, id, value1, value2);

    if (!assertInput(FUNC_NAME, value1, value2)) {
      return false;
    }

    auto posetAddRes = posetAddImpl(id, value1, value2);
    logPosetAdd(FUNC_NAME, id, value1, value2, getFirstBool(posetAddRes),
      getThirdBool(posetAddRes), getSecondBool(posetAddRes));

    return getFirstBool(posetAddRes);
  }

  // Jeżeli istnieje poset o identyfikatorze id, elementy value1 i value2
  // należą do tego zbioru, element value1 poprzedza element value2 oraz
  // usunięcie relacji między elementami value1 i value2 nie zaburzy warunków
  // bycia częściowym porządkiem, to usuwa relację między tymi elementami,
  // a w przeciwnym przypadku nic nie robi. Wynikiem jest true, gdy relacja
  // została zmieniona, a false w przeciwnym przypadku.
  bool poset_del(unsigned long id, char const *value1, char const *value2) {

    const static string FUNC_NAME = "poset_del";
    logInput(FUNC_NAME, id, value1, value2);

    if (!assertInput(FUNC_NAME, value1, value2)) {
      return false;
    }

    auto posetDelRes = posetDelImpl(id, value1, value2);
    logPosetDel(FUNC_NAME, id, value1, value2, getFirstBool(posetDelRes),
      getThirdBool(posetDelRes), getSecondBool(posetDelRes));

    return getFirstBool(posetDelRes);
  }

  // Jeżeli istnieje poset o identyfikatorze id, elementy value1 i value2
  // należą do tego zbioru oraz element value1 poprzedza element value2, to
  // wynikiem jest true, a w przeciwnym przypadku false.
  bool poset_test(unsigned long id, char const *value1, char const *value2) {

    const static string FUNC_NAME = "poset_test";
    logInput(FUNC_NAME, id, value1, value2);

    if (!assertInput(FUNC_NAME, value1, value2)) {
      return false;
    }

    auto posetTestRes = posetTestImpl(id, value1, value2);
    logPosetTest(FUNC_NAME, id, value1, value2,
      getFirstBool(posetTestRes), getThirdBool(posetTestRes),
      getSecondBool(posetTestRes));

    return getFirstBool(posetTestRes);
  }

  // Jeżeli istnieje poset o identyfikatorze id, usuwa wszystkie jego elementy
  // oraz relacje między nimi, a w przeciwnym przypadku nic nie robi.
  void poset_clear(unsigned long id) {

    const static string FUNC_NAME = "poset_clear";
    logInput(FUNC_NAME, id);

    auto posetClearRes = posetClearImpl(id);
    logPosetClear(FUNC_NAME, id, posetClearRes);

  }

}
