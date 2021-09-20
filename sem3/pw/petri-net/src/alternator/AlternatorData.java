package alternator;

import java.util.List;
import java.util.Map;
import petrinet.Transition;

class AlternatorData {

  final static String A = "A";
  final static String B = "B";
  final static String C = "C";
  final static String CRITICAL_SECTION = "criticalSection";

  private final static String SEMAPHORE = "SEMAPHORE";
  private final static String AVAILABILITY_OF_A = "availabilityOfA";
  private final static String AVAILABILITY_OF_B = "availabilityOfB";
  private final static String AVAILABILITY_OF_C = "availabilityOfC";
  private final static String CAN_I_EXIT_A = "canIExitA";
  private final static String CAN_I_EXIT_B = "canIExitB";
  private final static String CAN_I_EXIT_C = "canIExitC";

  private final static Integer ONE = 1;


  final static Map<String, Integer> INITIAL_MAP = Map.of(
      SEMAPHORE, ONE,
      A, ONE,
      B, ONE,
      C, ONE,
      AVAILABILITY_OF_A, ONE,
      AVAILABILITY_OF_B, ONE,
      AVAILABILITY_OF_C, ONE
  );

  //starting transitions
  Transition<String> startingTransitionA =
      new Transition<>(
          Map.of(
              A, ONE,
              AVAILABILITY_OF_A, ONE,
              SEMAPHORE, ONE),
          List.of(),
          List.of(),
          Map.of(
              CRITICAL_SECTION, ONE,
              CAN_I_EXIT_A, ONE)
      );

  Transition<String> startingTransitionB =
      new Transition<>(
          Map.of(
              B, ONE,
              AVAILABILITY_OF_B, ONE,
              SEMAPHORE, ONE),
          List.of(),
          List.of(),
          Map.of(
              CRITICAL_SECTION, ONE,
              CAN_I_EXIT_B, ONE)
      );

  Transition<String> startingTransitionC =
      new Transition<>(
          Map.of(
              C, ONE,
              AVAILABILITY_OF_C, ONE,
              SEMAPHORE, ONE),
          List.of(),
          List.of(),
          Map.of(
              CRITICAL_SECTION, ONE,
              CAN_I_EXIT_C, ONE)
      );


  //exit  transitions
  Transition<String> exitTransitionA =
      new Transition<>(
          Map.of(
              CRITICAL_SECTION, ONE,
              CAN_I_EXIT_A, ONE),
          List.of(AVAILABILITY_OF_B, AVAILABILITY_OF_C),
          List.of(),
          Map.of(
              SEMAPHORE, ONE,
              A, ONE,
              AVAILABILITY_OF_B, ONE,
              AVAILABILITY_OF_C, ONE)
      );

  Transition<String> exitTransitionB =
      new Transition<>(
          Map.of(
              CRITICAL_SECTION, ONE,
              CAN_I_EXIT_B, ONE),
          List.of(AVAILABILITY_OF_A, AVAILABILITY_OF_C),
          List.of(),
          Map.of(
              SEMAPHORE, ONE,
              B, ONE,
              AVAILABILITY_OF_A, ONE,
              AVAILABILITY_OF_C, ONE)
      );

  Transition<String> exitTransitionC =
      new Transition<>(
          Map.of(
              CRITICAL_SECTION, ONE,
              CAN_I_EXIT_C, ONE),
          List.of(AVAILABILITY_OF_A, AVAILABILITY_OF_B),
          List.of(),
          Map.of(
              SEMAPHORE, ONE,
              C, ONE,
              AVAILABILITY_OF_A, ONE,
              AVAILABILITY_OF_B, ONE)
      );

  List<Transition<String>> transitions = List.of(
      startingTransitionA, exitTransitionA,
      startingTransitionB, exitTransitionB,
      startingTransitionC, exitTransitionC);
}
