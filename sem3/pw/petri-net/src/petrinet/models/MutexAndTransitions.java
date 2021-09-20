package petrinet.models;

import java.util.List;
import java.util.concurrent.Semaphore;
import petrinet.Transition;

public class MutexAndTransitions<T> {

  private Semaphore mutex = new Semaphore(0);
  private List<Transition<T>> transition;

  public MutexAndTransitions(List<Transition<T>> transition) {
    this.transition = transition;
  }

  public Semaphore getMutex() {
    return mutex;
  }

  public List<Transition<T>> getTransition() {
    return transition;
  }
}
