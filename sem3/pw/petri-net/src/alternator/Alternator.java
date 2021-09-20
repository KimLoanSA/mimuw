package alternator;

import java.util.Map;
import java.util.Set;
import petrinet.PetriNet;

class Alternator extends AlternatorData {

  private final Long sleepTime;
  private PetriNet<String> petriNet;

  Alternator(Long sleepTime, boolean fair) {
    this.petriNet = new PetriNet<>(INITIAL_MAP, fair);
    this.sleepTime = sleepTime;
  }

  void run() {
    resolveReachable();
    simulate();
  }

  private void resolveReachable() {
    Set<Map<String, Integer>> reachable = petriNet.reachable(transitions);

    System.out.println(reachable.size());
    System.out.println("Warunek bezpieczenstwa: " + validateReachableStates(reachable));
  }

  private boolean validateReachableStates(Set<Map<String, Integer>> reachable) {
    return reachable.stream()
        .map(entry -> entry.getOrDefault(CRITICAL_SECTION, 0))
        .allMatch(place -> place <= 1);
  }

  private void simulate() {
    Thread threadA = new Thread(new AlternatorRunner(petriNet, startingTransitionA, exitTransitionA), A);
    Thread threadB = new Thread(new AlternatorRunner(petriNet, startingTransitionB, exitTransitionB), B);
    Thread threadC = new Thread(new AlternatorRunner(petriNet, startingTransitionC, exitTransitionC), C);

    threadA.start();
    threadB.start();
    threadC.start();

    try {
      Thread.sleep(sleepTime);

    } catch (Exception e) {
      System.err.println(e.getMessage());
    }

    threadA.interrupt();
    threadB.interrupt();
    threadC.interrupt();
    System.out.println("\n\n30 sekund - Skonczylem");
  }}
