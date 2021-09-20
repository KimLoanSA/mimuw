import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import petrinet.PetriNet;
import petrinet.Transition;

public class Validate {

  private static enum Place {
    A, B, C, D
  }

  private static <T> void putPositive(Map<T, Integer> map, T key, Integer value) {
    if (value > 0) {
      map.put(key, value);
    }
  }

  private static Map<Place, Integer> marking(int a, int b, int c, int d) {
    Map<Place, Integer> result = new HashMap<>();
    putPositive(result, Place.A, a);
    putPositive(result, Place.B, b);
    putPositive(result, Place.C, c);
    putPositive(result, Place.D, d);
    return result;
  }

  private static void error(int number) {
    System.out.println("ERROR " + number);
    System.exit(number);
  }

  public static void main(String[] args) {
    try {

      Map<Place, Integer> begin = marking(1, 1, 0, 0);
      Map<Place, Integer> end = marking(0, 0, 0, 1);

      PetriNet<Place> net = new PetriNet<>(begin, false);

      Map<Place, Integer> input = Collections.singletonMap(Place.A, 1);
      Collection<Place> reset = Collections.singleton(Place.B);
      Collection<Place> inhibitor = Collections.singleton(Place.C);
      Map<Place, Integer> output = Collections.singletonMap(Place.D, 1);

      Transition<Place> transition = new Transition<>(input, reset, inhibitor, output);
      Collection<Transition<Place>> transitions = Collections.singleton(transition);

      Set<Map<Place, Integer>> before = net.reachable(transitions);
      HashSet<Map<Place, Integer>> temp = new HashSet<>(Arrays.asList(begin, end));
      if (!before.equals(temp)) {
        error(1);
      }

      Transition<Place> fired = net.fire(transitions);

      if (fired != transition) {
        error(2);
      }

      Set<Map<Place, Integer>> after = net.reachable(Collections.emptySet());

      if (!after.equals(Collections.singleton(end))) {
        error(3);
      }

      System.out.println("OK");

    } catch (InterruptedException e) {
      error(4);
    }
  }

}
