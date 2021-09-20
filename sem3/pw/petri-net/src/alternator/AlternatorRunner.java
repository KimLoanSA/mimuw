package alternator;

import java.util.List;
import petrinet.PetriNet;
import petrinet.Transition;

class AlternatorRunner implements Runnable {

  private final static String DELIMETR = ".";

  private final PetriNet<String> petriNet;
  private final Transition<String> startingTransition;
  private final Transition<String> exitTransition;

  AlternatorRunner(PetriNet<String> petriNet, Transition<String> startingTransition,
      Transition<String> exitTransition) {
    this.petriNet = petriNet;
    this.startingTransition = startingTransition;
    this.exitTransition = exitTransition;
  }

  @Override
  public void run() {

    while (true) {
      try {
        petriNet.fire(List.of(startingTransition));
        print();
        petriNet.fire(List.of(exitTransition));
      } catch (InterruptedException e) {
        System.out.println(Thread.currentThread().getName() + " interruped");
        Thread.currentThread().interrupt();
        break;
      }
    }
  }

  private void print() {
    System.out.print(Thread.currentThread().getName());
    System.out.print(DELIMETR);
  }
}
