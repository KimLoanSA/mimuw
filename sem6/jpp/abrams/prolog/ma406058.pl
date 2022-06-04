% Marcin Abramowicz 406058
% entrypoint programu, wczytuje argumenty z lini polecenia i uruchamia algorytm
verify :-
  ensure_loaded(library(lists)),
  current_prolog_flag(argv, Argv),
  validateAndGetArguments(Argv, ThreadsN, ProgramPath),
  verify(ThreadsN, ProgramPath).


% === args ====================================================================
% wczytywanie i walidowanie liczby argumentow wejsciowych programu
validateAndGetArguments(Argv, _, _) :-
  length(Argv, ArgvN),
  ArgvN \= 3,
  write("Error: zla liczba argumentow! Oczekiwanio 2:\n"),
  write("<liczba watkow> <sciezka do pliku z programem>"),
  halt(1).

validateAndGetArguments(Argv, ThreadsN, ProgramPath) :-
  nth0(1, Argv, ThreadsNString),
  nth0(2, Argv, ProgramPath),
  atom_number(ThreadsNString, ThreadsN).


% === verify ==================================================================
% procedura z tresci zadania, ktora waliduje program
verify(ThreadsN, ProgramPath) :-
  op(700, xfx, <>),                                   % dodajemy znak <>
  validateAndRun(ThreadsN, ProgramPath).              % i odpalamy


% walidujemy parametry i odpalamu sprawdzanie
validateAndRun(ThreadsN, _) :-
  \+ validateNThreads(ThreadsN).                      % sprawdzamy ThreadsN

validateAndRun(_, ProgramPath) :-
  \+ readProgram(ProgramPath).                        % wczytujemy caly program

validateAndRun(ThreadsN, ProgramPath) :-
  validateNThreads(ThreadsN),                         % sprawdzamy ThreadsN
  readProgram(ProgramPath),                           % wczytujemy caly program
  runVerify(ThreadsN),                                % i walidujemy
  unload_file(ProgramPath).                           % i czyscimy po sobie


% walidacja parametru z liczba watkow - musi byc to liczba dodatnia
validateNThreads(ThreadsN) :-
  ThreadsN < 1,
  format("Error: parametr ~d powinien byc liczba > 0~n", [ThreadsN]),
  false.

validateNThreads(ThreadsN) :-
  ThreadsN >= 1.


% czyta tresc pliku z programem,
% albo wypisuje wiadomosc jesli plik nie istnieje i przerywa program
readProgram(ProgramPath) :-
  catch(consult(ProgramPath), _, printNoFileException(ProgramPath)).


%wypisuje wiadomosc jesli plik nie istnieje i przerywa program
printNoFileException(ProgramPath) :-
  format("Error: brak pliku o nazwie - ~w", ProgramPath),
  false.


% uruchamiamy walidacje
runVerify(ThreadsN) :-
  variables(Variables),                               % wczytujemy zmienne
  arrays(Arrays),                                     % wczytujemy tablice
  program(Program),                                   % wczytujemy kod programu
  initState(Variables, Arrays, ThreadsN, InitState),  % tworzymy stan startowy
  verifyAllSteps(Program, InitState, ThreadsN, Inv),  % uruchamiamy sprawdzenie
  printResults(Program, Inv).                         % i wypisujemy wyniki


% wypisujemy wynik - pusta lista, oznacza ze nie bylo zlych przeplotow
printResults(_, []) :-
  write("Program jest poprawny (bezpieczny).\n").

printResults(Program, [State|_]) :-
  printSectionInfo(Program, State).


% uruchamia rekurencyjne sprawdzenie programu:
% dla kazdego stanu odpalamy sprawdzenie dla kazdego mozliwego ruchu,
% czyli, iterujemy sie po id procesow i dla kazdego rekurencyjnie sprawdzmy
verifyAllSteps(Program, InputState, ThreadsN, Invalid) :-
  length(Program, ProgramLen),
  verifyAllSteps(
    Program,
    InputState,
    0,
    ThreadsN,
    ProgramLen,
    [],
    [],
    _,
    Invalid).


% kiedy zaczynamy iteracje (thread nr 0)
% dodajemy ten stan do kolekcji, poniewaz nizej w rekurencji
% nie chcemy ponownie rozpatrywac tego stanu,
% poniewaz zostanie on w pelni sprawdzony w tym wywolaniu
verifyAllSteps(Program,
              InputState,
              0,
              ThreadsN,
              ProgramLen,
              HistoryAcc,
              InvalidAcc,
              History,
              Invalid) :-
  isNotEndOfProgram(InputState, ProgramLen),   % sprawdzamy czy koniec programu
  isNotVisitedState(InputState, HistoryAcc),   % sprawdzamy czy juz bylismy tu
  getState(InputState, State),                 % bierzemy stan kroku
  verifyRecursive(                             % uruchamiamy sprawdzanie
    Program,
    InputState,
    0,
    ThreadsN,
    ProgramLen,
    [State|HistoryAcc],
    InvalidAcc,
    VerifyHistory,
    VerifyInvalid),
  verifyAllSteps(                              % i idziemy do nastepnego watku
    Program,
    InputState,
    1,
    ThreadsN,
    ProgramLen,
    VerifyHistory,
    VerifyInvalid,
    History,
    Invalid).

% jesli skonczylismy program lub bylismy juz w jakims stanie 
% to tu wyladujemy, wiec zwracamy wartosc akumulatora
verifyAllSteps(_, _, 0, _, _, HistoryAcc, InvalidAcc, HistoryAcc, InvalidAcc).

% sprawdzmy kolejne numery watkow,
% jesli jeszcze sie miescimy w ich liczbie to
% wywolujemy rekurencyjne sprawdzanie i idzemy dalej
verifyAllSteps(Program,
               InputState,
               ThreadId,
               ThreadsN,
               ProgramLen,
               HistoryAcc,
               InvalidAcc,
               History,
               Invalid) :-
  ThreadId < ThreadsN,
  verifyRecursive(
    Program,
    InputState,
    ThreadId,
    ThreadsN,
    ProgramLen,
    HistoryAcc,
    InvalidAcc,
    VerifyHistory,
    VerifyInvalid),
  NewThreadId is ThreadId + 1,
  verifyAllSteps(
    Program,
    InputState,
    NewThreadId,
    ThreadsN,
    ProgramLen,
    VerifyHistory,
    VerifyInvalid,
    History,
    Invalid).

% i tutaj trafimy jesli wyjdziemy z "iteracji"
verifyAllSteps(_, _, _, _, _, HistoryAcc, InvalidAcc, HistoryAcc, InvalidAcc).

% rekurencyjne wywolanie step -
% wykonujemy step, a potem odpalamy walidacje na
% poddrzewie z otrzymanym stanem
verifyRecursive(Program,
                InputState,
                ThreadId,
                ThreadsN,
                ProgramLen,
                HistoryAcc,
                InvalidAcc,
                History,
                Invalid) :-
  step(Program, InputState, ThreadId, OutputState, IsCorrect),
  callVerifyAllSteps(
    Program,
    OutputState,
    ThreadsN,
    ProgramLen,
    IsCorrect,
    HistoryAcc,
    InvalidAcc,
    History,
    Invalid).


% wywolanie dalszej rekurencji
callVerifyAllSteps(Program,
                   InputState,
                   ThreadsN,
                   ProgramLen,
                   true,
                   HistoryAcc,
                   InvalidAcc,
                   History,
                   Invalid) :-
  verifyAllSteps(
    Program,
    InputState,
    0,
    ThreadsN,
    ProgramLen,
    HistoryAcc,
    InvalidAcc,
    History,
    Invalid).

% lub zalezliczmy zly przeplot
callVerifyAllSteps(_,
                   InputState,
                   _,
                   _,
                   false,
                   HistoryAcc,
                   InvalidAcc,
                   HistoryAcc,
                   [InputState|InvalidAcc]).


% sprawdzenie czy dotarlismy do konca programu -
% jesli indeks jakiegokolwiek watku dotarl za program,
% to znaczy ze juz konczymy
isNotEndOfProgram(state(StepState, _), ProgramLen) :-
  isNotEndOfProgram(StepState, ProgramLen).

isNotEndOfProgram(single_step_state(_, _, Indexes), ProgramLength) :-
  isNotEndOfProgram(Indexes, ProgramLength).

isNotEndOfProgram([], _).

isNotEndOfProgram([Index|Indexes], ProgramLength) :-
  Index =< ProgramLength,
  isNotEndOfProgram(Indexes, ProgramLength).


% sprawdzenie czy bylismy juz w danym stanie -
% odczytanie z akumulatora, czy zawiera aktualny stan
isNotVisitedState(state(StepState, _), History) :-
  \+ member(StepState, History).


% === init ====================================================================
% tworzy typ `state` z polami:
% 1. aktualny stan typu single_step_state
% 2. historia ruchow (do odtworzenia przeplotu)
initState(Variables, Arrays, ThreadsN, state(SingleStepState, [])) :-
  initSingeStepState(Variables, Arrays, ThreadsN, SingleStepState).


% konstruktor typu `single_step_state`, z polami:
% 1. wartosci zmiennych
% 2. wartosci tablic
% 3. indeksy instrukcji watkow
initSingeStepState(Variables,
                   Arrays,
                   ThreadsN,
                   single_step_state(VariablesValues, ArraysValues, Indexes)) :-
  maplist(initVariableValue, Variables, VariablesValues),
  maplist(initArrayValue(ThreadsN), Arrays, ArraysValues),
  initListOfSize(ThreadsN, 1, Indexes).


% konstruktor `variable_value` - termu trzymajacego wartosc zmiennej
initVariableValue(VarName, variable_value(VarName, 0)).


% konstruktor `array_value` - termu trzymajacego wartosc tablicy
initArrayValue(ThreadsN, ArrayName, array_value(ArrayName, EmptyArray)) :-
  initListOfSize(ThreadsN, 0, EmptyArray).


% === step ====================================================================
step(Program, InputState, ThreadId, OutputState) :-
  step(Program, InputState, ThreadId, OutputState, _).

% krok weryfikatora - wykonanie jedej instrukcji:
% zapisanie ruchu i wykonianie instrukcji
step(Program, InputState, ThreadId, OutputState, IsCorrect) :-
  propageteStateToHistory(ThreadId, InputState, StateWithHistory),
  getPosition(StateWithHistory, ThreadId, Position),
  nth1(Position, Program, Instr),
  executeInstruction(
    Instr,
    Program,
    StateWithHistory,
    ThreadId,
    OutputState,
    IsCorrect).


% ewaluacja `assign(array(arrayName, index), val)`:
% ewaluujemy przypisywana wartosc,
% ewaluujemy wartosc indeksu,
% zapisujemy i idziemy krok dalej
executeInstruction(assign(array(ArrayName, ArrayIndex), Expr),
                   _,
                   InputState,
                   ThreadId,
                   OutputState,
                   true) :-
  evalAritm(Expr, ThreadId, InputState, Val),
  evalAritm(ArrayIndex, ThreadId, InputState, IndexVal),
  updateArrayValue(ArrayName, IndexVal, Val, InputState, OutputState1),
  moveToNextPosition(OutputState1, ThreadId, OutputState).

% ewaluacja `assign(varName, val)`:
% ewaluujemy przypisywana wartosc,
% zapisujemy i idziemy krok dalej
executeInstruction(assign(VarName, Expr),
                   _,
                   InputState,
                   ThreadId,
                   OutputState,
                   true) :-
  evalAritm(Expr, ThreadId, InputState, Val),
  updateVarValue(VarName, Val, InputState, OutputState1),
  moveToNextPosition(OutputState1, ThreadId, OutputState).

% ewaluacja `goto(pos)`
% ewaluujemy nowa pozycje,
% idziemy do podanej pozycji
executeInstruction(goto(NewPosition),
                   _,
                   InputState,
                   ThreadId,
                   OutputState,
                   true) :-
  evalAritm(NewPosition, ThreadId, InputState, NewPositionVal),
  updatePosition(NewPositionVal, InputState, ThreadId, OutputState).

% ewaluacja `condGoto(logExpr), pos)`
% ewaluujemy wyrazenie
% ewaluujemy nowa pozycje,
% i jesli wyrazenie jest prawdziwe to idziemy do podanej pozycji
% a w przeciwnym przypadku do nastepnej pozycji
executeInstruction(condGoto(LogExpr, NewPosition),
                   _,
                   InputState,
                   ThreadId,
                   OutputState,
                   true) :-
  evalLog(LogExpr, ThreadId, InputState, LogValue),
  LogValue,
    updatePosition(NewPosition, InputState, ThreadId, OutputState);
    moveToNextPosition(InputState, ThreadId, OutputState).

% ewaluacja `sekcja`
% sprawdzamy ile watkow aktualnie jest w sekcjach,
% jesli licznik jest > 1 to znaczy ze jest zle,
executeInstruction(sekcja,
                   Program,
                   InputState,
                   _,
                   InputState,
                   false) :-
  countThreadsInSection(Program, InputState, Counter),
  Counter > 1.

% ewaluacja `sekcja`, ale pozytywna sciezka -
% tylko 1 watek jest w sekcji, wiec idziemy do nastepnej pozycji
executeInstruction(sekcja,
                   Program,
                   InputState,
                   ThreadId,
                   OutputState,
                   true) :-
  countThreadsInSection(Program, InputState, Counter),
  Counter == 1,
  moveToNextPosition(InputState, ThreadId, OutputState).


% wrzucamy ruch do histori przeplotu
propageteStateToHistory(
    ThreadId,
    state(StepState, MovesHistory),
    state(StepState,[history(ThreadId, Position)|MovesHistory])) :-
  getPosition(StepState, ThreadId, Position).


% przesuwamy sie do najstepnej pozycji
moveToNextPosition(InputState, ThreadId, OutputState) :-
  getPosition(InputState, ThreadId, Position),
  NewPosition is Position + 1,
  updatePosition(NewPosition, InputState, ThreadId, OutputState).


% === history =================================================================
% wypisywanie komunikatu o znalezionym bledzie
printSectionInfo(Program, InputState) :-
  write("Program jest niepoprawny.\nNiepoprawny przeplot:\n"),
  printHistory(InputState),
  write("Procesy w sekcji:"),
  printThreadsInSection(Program, InputState).


% wypisywanie przeplotu
% omijamy pierwsza wartosc, poniewaz juz "weszlismy" do sekcji,
% a tego nie chcemy wypisywac
printHistory(state(_, [_|MovesHistory])) :-
  printHistory(MovesHistory).

printHistory([]).

printHistory([history(ThreadId, Move)|History]) :-
  printHistory(History),
  format("  Proces ~d: ~d~n", [ThreadId, Move]).


% wypisywanie przeplotu
printThreadsInSection(Program, InputState) :-
  getThreadsInSection(Program, InputState, ThreadsInSection),
  printThreadsInSection(ThreadsInSection).

printThreadsInSection([Index]) :-
  format(" ~d.", [Index]).

printThreadsInSection([Index|Indexes]) :-
  format(" ~d,", [Index]),
  printThreadsInSection(Indexes).


% zwraca watki w sekcji
getThreadsInSection(Program, state(StepState, _), Res) :-
  getThreadsInSection(Program, StepState, Res).

getThreadsInSection(Program, single_step_state(_, _, Indexes), Res) :-
  getThreadsInSection(Program, Indexes, Res).

getThreadsInSection(Program, Indexes, Res) :-
  getThreadsInSection(Program, Indexes, 0, [], Res).

getThreadsInSection(_, [], _, ResAcc, ResAcc).

% jesli watek jest w sekcji, dodajemy do numer watku do akumulatora
getThreadsInSection(Program, [Index|Indexes], ThreadId, ResAcc, Res) :-
  nth1(Index, Program, Inst),
  Inst == sekcja,
  NewThreadId is ThreadId + 1,
  getThreadsInSection(Program, Indexes, NewThreadId, [ThreadId|ResAcc], Res).

% wpp. przechodzimy dalej
getThreadsInSection(Program, [_|Indexes], ThreadId, ResAcc, Res) :-
  NewThreadId is ThreadId + 1,
  getThreadsInSection(Program, Indexes, NewThreadId, ResAcc, Res).


% liczy liczbe watkow w sekcji
countThreadsInSection(Program, state(StepState, _), Counter) :-
  countThreadsInSection(Program, StepState, Counter).

countThreadsInSection(Program, single_step_state(_, _, Indexes), Counter) :-
  countThreadsInSection(Program, Indexes, Counter).

countThreadsInSection(Program, Indexes, Counter) :-
  countThreadsInSection(Program, Indexes, 0, Counter).

countThreadsInSection(_, [], CounterAcc, CounterAcc).

% jesli watek jest w sekcji, zwiekszamy licznik
countThreadsInSection(Program, [Index|Indexes], CounterAcc, Counter) :-
  nth1(Index, Program, Inst),
  Inst == sekcja,
  CounterAcc1 is CounterAcc + 1,
  countThreadsInSection(Program, Indexes, CounterAcc1, Counter).

% wpp. przechodzimy dalej
countThreadsInSection(Program, [_|Indexes], CounterAcc, Counter) :-
  countThreadsInSection(Program, Indexes, CounterAcc, Counter).


% === eval ====================================================================
% ewaluacja `a + b`
evalAritm(Expr1+Expr2, ThreadId, State, Value) :-
  evalSimpleExpr(Expr1, ThreadId, State, Val1),
  evalSimpleExpr(Expr2, ThreadId, State, Val2),
  Value is Val1 + Val2.

% ewaluacja `a - b`
evalAritm(Expr1-Expr2, ThreadId, State, Value) :-
  evalSimpleExpr(Expr1, ThreadId, State, Val1),
  evalSimpleExpr(Expr2, ThreadId, State, Val2),
  Value is Val1 - Val2.

% ewaluacja `a * b`
evalAritm(Expr1*Expr2, ThreadId, State, Value) :-
  evalSimpleExpr(Expr1, ThreadId, State, Val1),
  evalSimpleExpr(Expr2, ThreadId, State, Val2),
  Value is Val1 * Val2.

% ewaluacja `a / b`
evalAritm(Expr1/Expr2, ThreadId, State, Value) :-
  evalSimpleExpr(Expr1, ThreadId, State, Val1),
  evalSimpleExpr(Expr2, ThreadId, State, Val2),
  Value is Val1 / Val2.

% ewaluacja zwyklego wyrazenia
evalAritm(SimpleExpr, ThreadId, State, Value) :-
  evalSimpleExpr(SimpleExpr, ThreadId, State, Value).


% ewaluacja `a < b`
evalLog(Expr1 < Expr2, ThreadId, State, Value) :-
  evalSimpleExpr(Expr1, ThreadId, State, Val1),
  evalSimpleExpr(Expr2, ThreadId, State, Val2),
  Val1 < Val2,
    Value = true;
    Value = false.

% ewaluacja `a  = b`
evalLog(=(Expr1, Expr2), ThreadId, State, Value) :-
  evalSimpleExpr(Expr1, ThreadId, State, Val1),
  evalSimpleExpr(Expr2, ThreadId, State, Val2),
  Val1 == Val2,
    Value = true;
    Value = false.

% ewaluacja `a <> b`
evalLog(\=(Expr1, Expr2), ThreadId, State, Value) :-
  evalSimpleExpr(Expr1, ThreadId, State, Val1),
  evalSimpleExpr(Expr2, ThreadId, State, Val2),
  Val1 \= Val2,
    Value = true;
    Value = false.


% ewaluacja prostego wyrazenia - liczba
evalSimpleExpr(Number, _, _, Value) :-
  number(Number),
  Value = Number.

% ewaluacja odwolania do tablicy
evalSimpleExpr(array(ArrayName, Index), ThreadId, State, Value) :-
  evalAritm(Index, ThreadId, State, IndexVal),
  getArrayValue(ArrayName, IndexVal, State, Value).

% ewaluacja odwolania do zmiennej lub `pid`
evalSimpleExpr(VarName, ThreadId, State, Value) :-
  \+ number(VarName),
  VarName == pid,
    Value = ThreadId;
    getVarValue(VarName, State, Value).


% === state ===================================================================
% zwraca pozycje danego watku
getPosition(state(StepState, _), ThreadId, Position) :-
  getPosition(StepState, ThreadId, Position).

getPosition(single_step_state(_, _, Indexes), ThreadId, Position) :-
  nth0(ThreadId, Indexes, Position).


% zwraca wartosc zmiennej
getVarValue(Name, state(StepState, _), Value) :-
  getVarValue(Name, StepState, Value).

getVarValue(Name, single_step_state(VariablesValues, _, _), Value) :- 
  getVarValue(Name, VariablesValues, Value).

getVarValue(Name, [variable_value(VarName, VarValue)|_], Value) :-
  Name == VarName,
  VarValue = Value.

getVarValue(Name, [variable_value(_, _)|List], Value) :-
  getVarValue(Name, List, Value).


% zwraca wartosc tablicy pod indeksem
getArrayValue(Name, Index, state(StepState, _), Value) :-
  getArrayValue(Name, Index, StepState, Value).

getArrayValue(Name, Index, single_step_state(_, ArraysValues, _), Value) :- 
  getArrayValue(Name, Index, ArraysValues, Value).

getArrayValue(Name, Index, [array_value(ArrayName, ArrayValue)|_], Value) :-
  Name == ArrayName,
  nth0(Index, ArrayValue, Value).

getArrayValue(Name, Index, [array_value(_, _)|List], Value) :-
  getArrayValue(Name, Index, List, Value).


% zwraca stan kroku
getState(state(StepState, _), StepState).


% aktualizuje pozycje
updatePosition(
    NewPosition,
    state(StepState, MovesHistory),
    ThreadId,
    state(UpdatedState, MovesHistory)) :-
  updatePosition(NewPosition, StepState, ThreadId, UpdatedState).

updatePosition(
    NewPosition,
    single_step_state(VariablesValues, ArraysValues, Indexes),
    ThreadId,
    single_step_state(VariablesValues, ArraysValues, UpadedIndexes)) :- 
  replaceNth0(Indexes, ThreadId, NewPosition, UpadedIndexes).


% aktualizuje wartosc zmiennej
updateVarValue(
    Name,
    Value,
    state(StepState, MovesHistory),
    state(UpdatedState, MovesHistory)) :-
  updateVarValue(Name, Value, StepState, UpdatedState).

updateVarValue(
    Name,
    Value,
    single_step_state(VariablesValues, ArraysValues, Indexes),
    single_step_state(UpdatedVariablesValues, ArraysValues, Indexes)) :-
  getVarIndex(Name, 0, VariablesValues, Index),
  NewElem = variable_value(Name, Value),
  replaceNth0(VariablesValues, Index, NewElem, UpdatedVariablesValues).


% zwraca indeks zmiennej
getVarIndex(Name, CurrentIndex, [variable_value(VarName, _)|_], Index) :-
  Name == VarName,
  Index = CurrentIndex.

getVarIndex(Name, CurrentIndex, [variable_value(_, _)|List], Index) :-
  NextIndex is CurrentIndex + 1,
  getVarIndex(Name, NextIndex, List, Index).


% aktualizuje wartosc tablicy pod indeksem
updateArrayValue(
    Name,
    Index,
    Value,
    state(StepState, MovesHistory),
    state(UpdatedState, MovesHistory)) :-
  updateArrayValue(Name, Index, Value, StepState, UpdatedState).

updateArrayValue(
    Name,
    Index,
    Value,
    single_step_state(VariablesValues, ArraysValues, Indexes),
    single_step_state(VariablesValues, UpdatedArraysValues, Indexes)) :-
  getArrayIndex(Name, ArraysValues, ArrayIndex),
  nth0(ArrayIndex, ArraysValues, CurrentArray),
  updateArrayValue(Name, Index, Value, CurrentArray, UpdatedArray),
  replaceNth0(ArraysValues, ArrayIndex, UpdatedArray, UpdatedArraysValues).

updateArrayValue(
    Name,
    Index,
    Value,
    array_value(_, CurrentArray),
    UpdatedArrayValue) :-
  replaceNth0(CurrentArray, Index, Value, UpdatedCurrentArray),
  UpdatedArrayValue = array_value(Name, UpdatedCurrentArray).


% zwraca indeks tabliby
getArrayIndex(Name, ArraysValues, Index) :-
  getArrayIndex(Name, 0, ArraysValues, Index).

getArrayIndex(Name, CurrentIndex, [array_value(ArrayName, _)|_], Index) :-
  Name == ArrayName,
  Index = CurrentIndex.

getArrayIndex(Name, CurrentIndex, [array_value(_, _)|List], Index) :-
  NextIndex is CurrentIndex + 1,
  getArrayIndex(Name, NextIndex, List, Index).


% === utils ===================================================================
% tworzy liste o podanej dlugosci z podanymi wartosciami
initListOfSize(Size, Value, Res) :-
  initListOfSize(Size, Value, [], Res).

initListOfSize(0, _, Acc, Acc).

initListOfSize(Size, Value, Acc, Res) :-
  NewSize is Size - 1,
  initListOfSize(NewSize, Value, [Value|Acc], Res).


% https://www.swi-prolog.org/pldoc/man?predicate=nth0/4
replaceNth0(List, Index, NewElem, NewList) :-
   nth0(Index, List, _, Transfer),
   nth0(Index, NewList, NewElem, Transfer).
