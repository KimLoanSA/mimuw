package multiplicator;


import java.util.List;
import java.util.Map;
import petrinet.Transition;

public class MultiplicatorData {

  private final static String FIRST_NUMBER = "firstNumber";
  private final static String SECOND_NUMBER = "secondNumber";
  final static String RESULT = "result";

  final static String PLACE_LEFT = "placeOne";
  final static String PLACE_RIGHT = "placeTwo";
  final static String PLACE_BOTTOM = "placeThree";

  final static Integer ONE = 1;

  final static String THREAD_ONE = "Thread 1";
  final static String THREAD_TWO = "Thread 2";
  final static String THREAD_THREE = "Thread 3";


  Map<String, Integer> buildInitialMap(Integer a, Integer b) {
    return Map.of(
        FIRST_NUMBER, a,
        SECOND_NUMBER, b,
        PLACE_LEFT, ONE);
  }

  private final static Transition<String> TRANSITION_LEFT = new Transition<>(
      Map.of(
          PLACE_BOTTOM, ONE,
          PLACE_LEFT, ONE),
      List.of(),
      List.of(),
      Map.of(
          SECOND_NUMBER, ONE,
          PLACE_LEFT, ONE
      )
  );

  private final static Transition<String> TRANSITION_TOP = new Transition<>(
      Map.of(PLACE_RIGHT, ONE),
      List.of(),
      List.of(SECOND_NUMBER),
      Map.of(PLACE_LEFT, ONE)
  );

  private final static Transition<String> TRANSITION_BOTTOM = new Transition<>(
      Map.of(
          FIRST_NUMBER, ONE,
          PLACE_LEFT, ONE),
      List.of(),
      List.of(PLACE_BOTTOM),
      Map.of(PLACE_RIGHT, ONE)
  );

  private final static Transition<String> TRANSITION_RIGHT = new Transition<>(
      Map.of(
          SECOND_NUMBER, ONE,
          PLACE_RIGHT, ONE),
      List.of(),
      List.of(),
      Map.of(
          RESULT, ONE,
          PLACE_BOTTOM, ONE,
          PLACE_RIGHT, ONE)
  );

  final static Transition<String> TRANSITION_RESULT = new Transition<>(
      Map.of(),
      List.of(),
      List.of(
          FIRST_NUMBER,
          PLACE_RIGHT,
          PLACE_BOTTOM
          ),
      Map.of()
  );


  final static List<Transition<String>> TRANSITIONS = List.of(
      TRANSITION_LEFT,
      TRANSITION_TOP,
      TRANSITION_BOTTOM,
      TRANSITION_RIGHT
  );
}
