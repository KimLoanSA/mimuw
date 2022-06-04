//
// Created by Anna Kalisz and Marcin Abramowicz
//

#include <iostream>
#include <string>
#include <utility>
#include <vector>
#include <regex>
#include <tuple>
#include <map>
#include <climits>

// ============= DANE ==========================================================


// Enum opisujacy rodzaj zapytania
enum RequestType {
  IN_NEW_TRAM_LINE,
  IN_NEW_TICKET,
  IN_QUERY,
  IN_END_OF_INPUT,
  IN_ERROR
};

// Enum opisujacy rodzaj odpowiedzi
enum ResponseType {
  OUT_CORRECT_ANSWER,
  OUT_NO_TICKETS,
  OUT_HAVE_TO_WAIT,
  OUT_NO_ERROR,
  OUT_ERROR,
  OUT_END_OF_INPUT
};

using string = std::string;
using pairII = std::pair<int, int>;
using pairSS = std::pair<std::string, std::string>;
using pairULUL = std::pair<unsigned long, unsigned long>;
using mapStringMapPairII = std::map<string, std::map<string, pairII>>;
using tupleStringULUL = std::tuple<string, unsigned long, unsigned long>;
using mapStringPairULUL = std::map<string, pairULUL>;

// Typ reprezetujacy czas
using myTime = pairII;
// Typ reprezetujacy rozklad jazdy tramwajow
using timetable = mapStringMapPairII;
// Typ reprezetujacy bilet
using ticket = tupleStringULUL;
// Typ reprezetujacy zbior biletow
using tickets = mapStringPairULUL;


// ============= PARSOWANIE ====================================================


// ------------- wejscie -------------------------------------------------------

// Funkcja czytajaca jedna linie i pomijajaca puste linie,
// w przypadku konca wejscia zwraca pustego stringa.
string readLine(unsigned long *lineNumber) {
  string result = "";

  while(std::getline(std::cin, result) && result.empty()) {
    (*lineNumber)++;
  }

  (*lineNumber)++;

  return result;
}

// Funkcja pomocnicza dopasowujaca wyrazenie regularne
bool matchRegex(const string &line, const string &pattern) {
  return std::regex_match(line, std::regex(pattern));
}

// Funkcja sprawdzajaca poprawnosc skladnii lini i okreslajaca rodzaj zapytania
RequestType validateLine(const string &line) {
  const static string NEW_TRAM_LINE_REGEX_PATTERN =
    "^(0*)(\\d+)( [1-2]{0,1}[0-9]:[0-6][0-9] [a-zA-Z_^]+)+$";
  const static string NEW_TICKET_REGEX_PATTERN =
    "^[a-zA-Z ]+ (\\d+)\\.\\d{2} [1-9]\\d*$";
  const static string QUERY_REGEX_PATTERN =
    "^\\? [a-zA-Z_^]+( (0*)\\d+ [a-zA-Z_^]+)+$";

  // String jest pusty czyli wejscie sie skonczylo.
  if (line.empty()) {
    return IN_END_OF_INPUT;
  }

  if (matchRegex(line, NEW_TRAM_LINE_REGEX_PATTERN)) {
    return IN_NEW_TRAM_LINE;
  }

  if (matchRegex(line, NEW_TICKET_REGEX_PATTERN)) {
    return IN_NEW_TICKET;
  }

  if (matchRegex(line, QUERY_REGEX_PATTERN)) {
    return IN_QUERY;
  }

  // Nie pasuje do zadnego wzorca co oznacza ze linia jest niepoprawna.
  return IN_ERROR;
}

// Funkcja konwertujaca z pary stringow na pare intow
myTime stringToHour(const string &stringHour, const string &stringMinutes) {
  return {std::stoi(stringHour), std::stoi(stringMinutes)};
}

// Funkcja sprawdzajaca czy godzina miesci sie miedzy 5:55, a 21:21
// i jest popawna godzina
bool validateHour(const myTime &timeToVerify) {
  const static myTime THE_EARLIEST_HOUR = {5, 55};
  const static myTime THE_LATEST_HOUR = {21, 21};

  return timeToVerify.second >= 0
    && timeToVerify.second < 60
    && timeToVerify >= THE_EARLIEST_HOUR
    && timeToVerify <= THE_LATEST_HOUR;
}

// Funkcja przetwarzajaca jedna linie
std::pair<RequestType, string> parseLine(unsigned long *lineNumber) {
  string inputLine = readLine(lineNumber);

  return {validateLine(inputLine), inputLine};
}

// Funkcja wyszukujaca wzorzec
std::smatch regexSearch(const string &input, const string &pattern) {
  std::smatch resultMatch;
  std::regex_search(input, resultMatch, std::regex(pattern));

  return resultMatch;
}

// ------------- NEW LINE ------------------------------------------------------

// Funkcja ekstraktujaca numer lini tramwajowej z lini wejsciowej
string extractTramLineNumber(const string &inputLine) {
  const string TRAM_LINE_NUMBER_REGEX_PATTERN = "^(0*)(\\d+)";

  return regexSearch(inputLine, TRAM_LINE_NUMBER_REGEX_PATTERN).str(2);
}

// Funkcja ekstraktujaca nazwe przystanku i godzine przyjazdu
std::tuple<ResponseType, string, myTime> extractTramLineStopNameAndHour(
  const std::smatch &match) {

  myTime hour = stringToHour(match.str(1), match.str(2));
  ResponseType responseType = validateHour(hour) ? OUT_NO_ERROR : OUT_ERROR;

  return {responseType, match.str(3), hour};
}

// Funkcja zwraca pare zawierajaca drugi i trzeci element krotki
std::pair<string, myTime> extractStopNameAndHourFromTuple(
  const std::tuple<ResponseType, string, myTime> &tuple) {

  return {std::get<1>(tuple), std::get<2>(tuple)};
}

// Funkcja ekstraktujaca numer lini i rozklad jazdy lini tramwajowej
// z lini wejsciowej
std::pair<ResponseType, std::vector<std::pair<string, myTime>>>
extractTramLineStopNameAndHourVector(const string &inputLine) {

  const static string TRAM_LINE_TIMETABLE_REGEX_PATTERN =
    "([1-2]{0,1}[0-9]):([0-6][0-9]) ([a-zA-Z_^]+)";
  std::regex timetableRegex(TRAM_LINE_TIMETABLE_REGEX_PATTERN);

  std::vector<std::pair<string, myTime>> resultTimetable;

  auto regexBegin =
    std::sregex_iterator(inputLine.begin(), inputLine.end(), timetableRegex);
  auto regexEnd = std::sregex_iterator();

  for (auto regexIt = regexBegin; regexIt != regexEnd; regexIt++) {
    auto stopToAdd = extractTramLineStopNameAndHour(*regexIt);

    if (std::get<0>(stopToAdd) == OUT_ERROR) {
      resultTimetable.clear();

      return {OUT_ERROR, resultTimetable};
    }

    resultTimetable.push_back(extractStopNameAndHourFromTuple(stopToAdd));
  }

  return {OUT_NO_ERROR, resultTimetable};
}

// Funkcja ekstraktujaca elementy z lini wejsciowej dla zapytania
// dodania nowej lini tramwajowej
std::tuple<ResponseType, string, std::vector<std::pair<string, myTime>>>
  parseLineForNewTramLine(const string &inputLine) {

  auto tramLineTimetable = extractTramLineStopNameAndHourVector(inputLine);
  string tramLineNumber = extractTramLineNumber(inputLine);

  return {std::get<0>(tramLineTimetable), tramLineNumber,
          std::get<1>(tramLineTimetable)};
}

// ------------- NEW TICKET ----------------------------------------------------

// Funkcja sprawdzajaca wielkosc podanej ceny na wejscu
bool validatePrice(const string &price) {
  const static string maxValue = std::to_string(ULONG_MAX);

  return price.size() < maxValue.size() || price.compare(maxValue) <= 0;
}

// Funkcja ekstraktujaca elementy z lini wejsciowej dla zapytania o
// dodanie nowego biletu
std::pair<ResponseType, ticket> parseLineForNewTicket(const string &inputLine) {

  const static string TICKET_NAME_REGEX_PATTERN = "^([a-zA-Z ]+) ";
  const static string TICKET_PRICE_REGEX_PATTERN = "(\\d+)\\.(\\d{2})";
  const static string TICKET_DURATION_REGEX_PATTERN = "\\d+$";
  const static std::pair<ResponseType, ticket> ERROR_RESPONSE =
    {OUT_ERROR, {"", 1, 1}};

  std::smatch ticketNameMatch =
    regexSearch(inputLine, TICKET_NAME_REGEX_PATTERN);
  std::smatch ticketPriceMatch =
    regexSearch(inputLine, TICKET_PRICE_REGEX_PATTERN);
  std::smatch ticketDurationMatch =
    regexSearch(inputLine, TICKET_DURATION_REGEX_PATTERN);

  string ticketPriceString = ticketPriceMatch.str(1) + ticketPriceMatch.str(2);

  if (validatePrice(ticketPriceString)) {
    return {OUT_NO_ERROR,
            {ticketNameMatch.str(1),
             std::stoul(ticketPriceString),
             std::stoul(ticketDurationMatch.str())}};
  }

  return ERROR_RESPONSE;
}

// ------------- QUERY ---------------------------------------------------------

// Funkcja ekstraktujaca nazwe przystanku i numer linii
pairSS extractStopNameAndLineNumber(const std::smatch &match) {
  return {match.str(3), match.str(1)};
}

// Funkcja ekstraktujaca elementy z lini wejsciowej dla zapytania o trase
std::vector<pairSS> parseLineForQuery(const string &inputLine) {

  const static string QUERY_LAST_STOP_REGEX_PATTERN = "([a-zA-Z_^]+)$";
  const static string QUERY_STOP_LINE_REGEX_PATTERN =
    "([a-zA-Z_^]+) (0*)(\\d+) ";

  std::vector<pairSS> resultVector;
  std::regex stopAndLineNumberRegex(QUERY_STOP_LINE_REGEX_PATTERN);

  auto regexBegin =
    std::sregex_iterator(inputLine.begin(), inputLine.end(),
      stopAndLineNumberRegex);
  auto regexEnd = std::sregex_iterator();

  for (auto regexIt = regexBegin; regexIt != regexEnd; regexIt++) {
    resultVector.push_back(extractStopNameAndLineNumber(*regexIt));
  }

  std::smatch queryLastStopMatch =
    regexSearch(inputLine, QUERY_LAST_STOP_REGEX_PATTERN);
  //ostatni przystanek
  resultVector.push_back({"", queryLastStopMatch.str()});

  return resultVector;
}

// ------------- wyjscie -------------------------------------------------------

// Funkcja wypisujaca na standardowe wyjscie zestaw biletow na dana trase
// w formacie:
// ! {ticketName1}; {ticketName2}; ... {ticketNameN}
void printAnswer(const string &answer) {
  const static string CORRECT_ANSWER_PREFIX = "!";

  std::cout << CORRECT_ANSWER_PREFIX << " " << answer << std::endl;
}

// Funkcja wypisujaca na standardowe wyjscie informacje o koniecznosci
// czekania na podanym przystanku w formacie:
// :-( {stopName}
void printHaveToWait(const string &stopName) {
  const static string HAVE_TO_WAIT_PREFIX = ":-(";

  std::cout << HAVE_TO_WAIT_PREFIX << " " << stopName << std::endl;
}

// Funkcja wypisujaca na standardowe wyjscie informacje o braku mozliwosci
// kupna biletow na podana trase w formacie:
// :-|
void printNoTickets() {
  const static string NO_TICKETS_MESSAGE = ":-|";

  std::cout << NO_TICKETS_MESSAGE << std::endl;
}

// Funkcja wypisujaca na standardowe wyjscie liczbe wszystkich proponowanych
// biletow
void printTicketCounter(const unsigned long ticketsCounter) {
  std::cout << ticketsCounter << std::endl;
}

// Funkcja wypisujaca error na standardowe wyjscie diagnostyczne w formacie:
// Error in line {lineNumber}: {line}
void printError(const unsigned long lineNumber, const string &line) {
  const static string ERROR_LINE_PREFIX = "Error in line";

  std::cerr << ERROR_LINE_PREFIX << " " << lineNumber
    << ": " << line << std::endl;
}

// ------------- rozwiklywanie wyjscia -----------------------------------------

void resolveOutput(ResponseType responseType, const string &inputLine,
  const unsigned long ticketsCounter, const unsigned long lineNumber) {

  if (responseType == OUT_CORRECT_ANSWER) {
    printAnswer(inputLine);
  }

  if (responseType == OUT_HAVE_TO_WAIT) {
    printHaveToWait(inputLine);
  }

  if (responseType == OUT_NO_TICKETS) {
    printNoTickets();
  }

  if (responseType == OUT_ERROR) {
    printError(lineNumber, inputLine);
  }

  if (responseType == OUT_END_OF_INPUT) {
    printTicketCounter(ticketsCounter);
  }
}


// ============= PRZETWARZANIE =================================================

// Funkcja zwraca nazwe biletu
string getTicketName(const ticket &ticket) {
  return std::get<0>(ticket);
}

// Funkcja zwraca cene biletu
unsigned long getTicketPrice(const ticket &ticket) {
  return std::get<1>(ticket);
}

// Funkcja zwraca dlugosc trwania biletu
unsigned long getTicketDuration(const ticket &ticket) {
  return std::get<2>(ticket);
}

// ------------- NEW LINE ------------------------------------------------------

// Funkcja sprawdzajaca czy dana linia juz istnieje
bool isLineExists(timetable &timetable, const string &tramLineNumber) {
  return timetable.count(tramLineNumber);
}

// Funkcja sprawdzajaca czy przystanek jest w trasie
bool insertToTimetable(std::map <string, myTime> &tramLine,
  std::pair <string, myTime> &station) {

  return !tramLine.insert(station).second;
}

bool areHoursIncreasing(const myTime &hour1, const myTime &hour2) {
  return hour1 >= hour2;
}

// Funkcja dodaje nową linię do rozkładu. Jeśli dana linia już istnieje,
// zwraca false. W przeciwnym razie zwraca true. Zakłada poprawność parametrów.
ResponseType addLine(timetable &timetable, const string &tramLineNumber,
  std::vector<std::pair<string, myTime>> &stationsTimes) {

  if (isLineExists(timetable, tramLineNumber)) {
    return OUT_ERROR;
  }

  std::map<string, myTime> newTramLine;
  size_t stationTimesSize = stationsTimes.size();

  for (size_t i = 0; i < stationTimesSize; i++) {
    if (i + 1 < stationTimesSize
      && areHoursIncreasing(stationsTimes[i].second,
        stationsTimes[i + 1].second)) {

      return OUT_ERROR;
    }

    if (insertToTimetable(newTramLine, stationsTimes[i])) {
      newTramLine.clear();

      return OUT_ERROR;
    }
  }

  return timetable.insert({tramLineNumber, newTramLine}).second
    ? OUT_NO_ERROR : OUT_ERROR;
}

// ------------- NEW TICKET ----------------------------------------------------

// Funckja sprawdza, czy bilet o danej nazwie znajduje się w mapie.
bool isTicketAdded(const string &ticketName, tickets &tickets) {
  return tickets.count(ticketName);
}

// Funkcja dodaje bilet do mapy na bilety.
ResponseType addTicket(tickets &tickets, const ticket &newTicket) {

  if (isTicketAdded(getTicketName(newTicket), tickets)) {
    return OUT_ERROR;
  }

  std::pair<string, pairULUL> newElement =
    {getTicketName(newTicket),
     {getTicketPrice(newTicket), getTicketDuration(newTicket)}};

  return tickets.insert(newElement).second ? OUT_NO_ERROR : OUT_ERROR;
}

// ------------- QUERY ---------------------------------------------------------

// Funkcja zwraca roznice czasow
long diffTimes(const myTime &departure, const myTime &arrival) {
  return (arrival.second - departure.second) + 60L *
    (arrival.first - departure.first);
}

// Funkcja zwraca nazwe stacji
string getStationLine(const pairSS &station) {
  return station.first;
}

// Funkcja zwraca czas przyjazdu na stacje
string getStationName(const pairSS &station) {
  return station.second;
}

//  Funkcja sprawdza poprawnosc danych dla zapytania
ResponseType validateForWrongData(timetable &timetable,
  const std::vector<pairSS> &numbersStations) {

  const static myTime WRONG_TIME = {-1, -1};
  size_t size = numbersStations.size() - 1;
  myTime prevArrivalTime = WRONG_TIME;

  for (size_t i = 0; i < size; i++) {
    auto lineIt = timetable.find(getStationLine(numbersStations[i]));
    // brak lini tramwajowej
    if (lineIt == timetable.end()) {
      return OUT_ERROR;
    }

    auto oneLineTimetable = lineIt->second;
    auto departureTimeIt =
      oneLineTimetable.find(getStationName(numbersStations[i]));
    // nie ma przystanku w lini tramwajowej
    if (departureTimeIt == oneLineTimetable.end()) {
      return OUT_ERROR;
    }

    auto arrivalTimeIt =
      oneLineTimetable.find(getStationName(numbersStations[i + 1]));
    // nie ma nastepnego przystanku w lini tramwajowej
    if (arrivalTimeIt == oneLineTimetable.end()) {
      return OUT_ERROR;
    }

    // przyjedziemy na przystanek po odjezdzie
    if (departureTimeIt->second < prevArrivalTime) {
      return OUT_ERROR;
    }

    // zakladajac ze  w lini tramwajowej godziny sa rosnace to sprawdzamy czy
    // czy szukamy przystankow w dobrej kolejnosci
    if (departureTimeIt->second >= arrivalTimeIt->second) {
      return OUT_ERROR;
    }

    prevArrivalTime = arrivalTimeIt->second;
  }

  return OUT_NO_ERROR;
}

// Funkcja sprawdza czy na jakims przystanku trzeba bedzie czekac
std::pair<ResponseType, string> validateForWaiting(timetable &timetable,
  const std::vector<pairSS> &numbersStations) {

  const static myTime WRONG_TIME = {-1, -1};
  const static std::pair<ResponseType, string> OK_RESPONSE = {OUT_NO_ERROR, ""};
  size_t size = numbersStations.size() - 1;
  myTime prevArrivalTime = WRONG_TIME;

  for (size_t i = 0; i < size; i++) {
    auto lineIt = timetable.find(getStationLine(numbersStations[i]));
    auto oneLineTimetable = lineIt->second;

    auto departureTimeIt =
      oneLineTimetable.find(getStationName(numbersStations[i]));
    auto arrivalTimeIt =
      oneLineTimetable.find(getStationName(numbersStations[i + 1]));

    //  nie jest to pierwszy przystanek i czasy przyjazdu i odjazdu nie sa rowne
    if (departureTimeIt->second != prevArrivalTime &&
        prevArrivalTime != WRONG_TIME) {
      return {OUT_HAVE_TO_WAIT, getStationName(numbersStations[i])};
    }

    prevArrivalTime = arrivalTimeIt->second;
  }

  return OK_RESPONSE;
}

// Funkcja liczy czas trasy
long countTime(timetable &timetable,
  const std::vector<pairSS> &numbersStations) {

  const static myTime WRONG_TIME = {-1, -1};

  long time = 0;
  size_t size = numbersStations.size() - 1;
  myTime prevArrivalTime = WRONG_TIME;

  for (size_t i = 0; i < size; i++) {
    auto lineIt = timetable.find(getStationLine(numbersStations[i]));
    auto oneLineTimetable = lineIt->second;

    auto departureTimeIt =
      oneLineTimetable.find(getStationName(numbersStations[i]));
    auto arrivalTimeIt =
      oneLineTimetable.find(getStationName(numbersStations[i + 1]));

    time += diffTimes(departureTimeIt->second, arrivalTimeIt->second);

    prevArrivalTime = arrivalTimeIt->second;
  }

  return time;
}

// Funkcja waliduje trase oraz zwraca łączny czas podróży dla trasy
std::tuple<ResponseType, unsigned long, string> validateAndGetTravelTime(
  timetable &timetable, const std::vector<pairSS> &numbersStations) {

  const static std::tuple<ResponseType, unsigned long, string> ERROR_RESPONSE =
    {OUT_ERROR, 0, ""};

  if (validateForWrongData(timetable, numbersStations) == OUT_ERROR) {
    return ERROR_RESPONSE;
  }

  auto validationForWaiting = validateForWaiting(timetable, numbersStations);
  if (validationForWaiting.first == OUT_HAVE_TO_WAIT) {
    return {OUT_HAVE_TO_WAIT, 0, validationForWaiting.second};
  }

  long time = countTime(timetable, numbersStations);

  return {OUT_CORRECT_ANSWER, time, ""};
}

// Funkcja zwracajaca nazwe biletu
string getTicketItName(const tickets::iterator &ticketIt) {
  return ticketIt->first;
}

// Funkcja zwracajaca czas trwania biletu
long getTicketItTime(const tickets::iterator &ticketIt) {
  return ticketIt->second.second;
}

// Funkcja zwracajaca cene biletu
unsigned long getTicketInPrice(const tickets::iterator &ticketIt) {
  return ticketIt->second.first;
}

unsigned long updateTicketSet(const tickets::iterator &ticketIt1,
  const tickets::iterator &ticketIt2, const tickets::iterator &ticketIt3,
  const long time, unsigned long bestPrice, std::vector<ticket> &ticketSet) {

  long wholeTime = getTicketItTime(ticketIt1) + getTicketItTime(ticketIt2)
    + getTicketItTime(ticketIt3);

  unsigned long wholePrice = getTicketInPrice(ticketIt1)
    + getTicketInPrice(ticketIt2) + getTicketInPrice(ticketIt3);

  if (wholeTime > time && wholePrice < bestPrice) {
    bestPrice = wholePrice;

    ticket tuple1 = {getTicketItName(ticketIt1),
                     getTicketInPrice(ticketIt1),
                     getTicketItTime(ticketIt1)};
    ticket tuple2 = {getTicketItName(ticketIt2),
                     getTicketInPrice(ticketIt2),
                     getTicketItTime(ticketIt2)};
    ticket tuple3 = {getTicketItName(ticketIt3),
                     getTicketInPrice(ticketIt3),
                     getTicketItTime(ticketIt3)};

    ticketSet = {tuple1, tuple2, tuple3};
  }

  return bestPrice;
}

// Funkcja szukajaca najlepszego zestawu biletow
std::vector<ticket> bestTicketSet(long time, tickets &tickets) {
  auto ticketsBegin = tickets.begin();
  auto ticketsEnd = tickets.end();
  std::vector<ticket> ticketSet;
  unsigned long bestPrice = ULONG_MAX;

  for (auto ticketIt1 = ticketsBegin; ticketIt1 != ticketsEnd; ticketIt1++) {
    for (auto ticketIt2 = ticketIt1; ticketIt2 != ticketsEnd; ticketIt2++) {
      for (auto ticketIt3 = ticketIt2; ticketIt3 != ticketsEnd; ticketIt3++) {

        bestPrice = updateTicketSet(ticketIt1, ticketIt2, ticketIt3,
          time, bestPrice, ticketSet);
      }
    }
  }

  return ticketSet;
}

// Funkcja laczaca nazwy biletow w jeden string
string mergeTicketsNames(std::vector<ticket> &tickets) {
  const static string SEPARATOR = "; ";
  string result = "";
  size_t ticketsSize = tickets.size();

  for (size_t i = 0; i < ticketsSize; i++) {
    string actualString = getTicketName(tickets[i]);

    if (!actualString.empty()) {
      result += actualString;

      if (i + 1 < ticketsSize) {
        result += SEPARATOR;
      }
    }
  }

  return result;
}

// Funkcja liczaca proponowane bilety
unsigned long countTickets(const std::vector<ticket> &tickets) {
  unsigned long result = 0;

  for (const auto& ticket : tickets) {
    if (!getTicketName(ticket).empty()) {
      result++;
    }
  }

  return result;
}

// Funkcja obslugujaca zapytanie
std::pair<ResponseType, string> query(timetable &timetable, tickets &tickets,
  std::vector<pairSS> &stopsTimeAndNames, unsigned long *ticketsCounter) {

  auto validatedData = validateAndGetTravelTime(timetable, stopsTimeAndNames);

  // walidacja sie nie powiodla
  if (std::get<0>(validatedData) != OUT_CORRECT_ANSWER) {
    return {std::get<0>(validatedData), std::get<2>(validatedData)};
  }

  auto result = bestTicketSet(std::get<1>(validatedData), tickets);
  unsigned long countedTickets = countTickets(result);

  if (countedTickets == 0) {
    return {OUT_NO_TICKETS, ""};
  }

  (*ticketsCounter) += countedTickets;

  return {OUT_CORRECT_ANSWER, mergeTicketsNames(result)};
}

// ------------- rozwiklywanie zapytan -----------------------------------------

// Funkcja rozwiazujaca dodawanie nowej lini
std::pair<ResponseType, string> resolveLineNewTramLine(timetable &timetable,
  const string &inputLine) {

  auto request = parseLineForNewTramLine(inputLine);

  if (std::get<0>(request) == OUT_ERROR) {
    return {OUT_ERROR, inputLine};
  }

  return {addLine(timetable, std::get<1>(request), std::get<2>(request)),
          inputLine};
}

// Funkcja przetwarzajaca linie danego rodzaju zapytania
std::pair<ResponseType, string> resolveLineNewTicket(tickets &tickets,
  const string &inputLine) {

  auto request = parseLineForNewTicket(inputLine);

  if (request.first == OUT_ERROR) {
    return {OUT_ERROR, inputLine};
  }

  return {addTicket(tickets, request.second), inputLine};

}

// Funkcja przetwarzajaca zapytanie
std::pair<ResponseType, string> resolveLineQuery(timetable &timetable,
  tickets &tickets, const string &inputLine, unsigned long *ticketsCounter) {

  auto request = parseLineForQuery(inputLine);
  auto response = query(timetable, tickets, request, ticketsCounter);

  if (response.first == OUT_ERROR) {
    return {OUT_ERROR, inputLine};
  }

  return response;
}

// Funkcja przetwarzajaca linie danego rodzaju zapytania
std::pair<ResponseType, string> resolveLine(timetable &timetable,
  tickets &tickets, RequestType requestType, const string &inputLine,
  unsigned long *ticketsCounter) {

  if (requestType == IN_ERROR) {
  	return {OUT_ERROR, inputLine};
  }

  if (requestType == IN_NEW_TRAM_LINE) {
    return resolveLineNewTramLine(timetable, inputLine);
  }

  if (requestType == IN_NEW_TICKET) {
    return resolveLineNewTicket(tickets, inputLine);
  }

  if (requestType == IN_QUERY) {
    return resolveLineQuery(timetable, tickets, inputLine, ticketsCounter);
  }

  if (requestType == IN_END_OF_INPUT) {
    return {OUT_END_OF_INPUT, inputLine};
  }

  return {OUT_ERROR, inputLine};
}

// Funkcja przetwarzajaca dane i obliczajaca wyniki
void process(timetable &timetable, tickets &tickets) {

  const static std::pair<string, pairULUL> EMPTY_TICKET = {"", {0, 0}};
  tickets.insert(EMPTY_TICKET); //pusty bilet zeby sprawdzac rowniez dla 1 i 2

  unsigned long lineNumber = 0;
  unsigned long ticketsCounter = 0;
  for (auto input = parseLine(&lineNumber);; input = parseLine(&lineNumber)) {

    auto response = resolveLine(timetable, tickets, std::get<0>(input),
      std::get<1>(input), &ticketsCounter);

    resolveOutput(response.first, response.second, ticketsCounter, lineNumber);

    if (response.first == OUT_END_OF_INPUT) {
      return;
    }
  }
}

// ============= MAIN ==========================================================

int main() {
  timetable timetable;
  tickets tickets;

  process(timetable, tickets);

  return 0;
}