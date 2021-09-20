package multiplicator;

import java.util.List;
import petrinet.PetriNet;
import petrinet.Transition;

public class MultiplicatorRunner implements Runnable {

  private final PetriNet<String> petriNet;
  private final List<Transition<String>> transitions;
  private int counter = 0;

  public MultiplicatorRunner(PetriNet<String> petriNet, List<Transition<String>> transitions) {
    this.petriNet = petriNet;
    this.transitions = transitions;
  }

  @Override
  public void run() {
    while (true) {
      try {
        petriNet.fire(transitions);
        counter++;
      } catch (InterruptedException e) {
        System.out.println(Thread.currentThread().getName() + " interrupted: " + counter + " fire calls");
        break;
      }
    }
  }
}
