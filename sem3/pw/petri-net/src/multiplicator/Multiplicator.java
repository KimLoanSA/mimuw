package multiplicator;

import java.util.List;
import java.util.Map;
import java.util.Set;
import petrinet.PetriNet;

public class Multiplicator extends MultiplicatorData {

  private final PetriNet<String> petriNet;

  public Multiplicator(Integer a, Integer b, boolean fair) {
    this.petriNet = new PetriNet<>(buildInitialMap(a, b), fair);
  }

  void run() {
    Thread thread1 = new Thread(new MultiplicatorRunner(petriNet, TRANSITIONS), THREAD_ONE);
    Thread thread2 = new Thread(new MultiplicatorRunner(petriNet, TRANSITIONS), THREAD_TWO);
    Thread thread3 = new Thread(new MultiplicatorRunner(petriNet, TRANSITIONS), THREAD_THREE);

    thread1.start();
    thread2.start();
    thread3.start();

    try {
      petriNet.fire(List.of(TRANSITION_RESULT));
    } catch (InterruptedException e) {
      System.err.println("Main interrupted");
    }

    System.out.println(getResult(petriNet.reachable(List.of())));

    thread1.interrupt();
    thread2.interrupt();
    thread3.interrupt();
  }

  Integer getResult(Set<Map<String, Integer>> reachable) {
    return reachable.stream()
        .map(state -> state.getOrDefault(RESULT, 0))
        .findFirst()
        .get();
  }


}
