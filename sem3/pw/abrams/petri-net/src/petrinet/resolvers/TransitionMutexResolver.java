package petrinet.resolvers;

import java.util.Deque;
import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;
import petrinet.Transition;
import petrinet.models.MutexAndTransitions;
import petrinet.models.State;

public class TransitionMutexResolver<T> {

  private Deque<MutexAndTransitions<T>> mutexAndTransitionsList = new LinkedList<>();
  private boolean fair;
  private State<T> actualState;
  private AtomicBoolean isActive = new AtomicBoolean();

  public TransitionMutexResolver(State<T> actualState, boolean fair) {
    this.fair = fair;
    this.actualState = actualState;
  }

  public synchronized MutexAndTransitions<T> getMutex(List<Transition<T>> transitions) {
    MutexAndTransitions<T> mutexAndTransition = new MutexAndTransitions<>(transitions);

    actualState
        .findFirstTransitionEnabled(transitions)
        .ifPresentOrElse(
            o -> mutexAndTransitionsList.addFirst(mutexAndTransition),
            () -> mutexAndTransitionsList.addLast(mutexAndTransition));

    releaseEnabledOne();

    return mutexAndTransition;
  }

  public synchronized void updateStateAndReleaseNext(State<T> state) {
    actualState = state;
    isActive.set(false);

    releaseEnabledOne();
  }

  public synchronized void removeFromQueue(MutexAndTransitions<T> mutexAndTransitions) {
    mutexAndTransitionsList.remove(mutexAndTransitions);
  }

  private synchronized void releaseEnabledOne() {
    mutexAndTransitionsList
        .stream()
        .filter(o -> !isActive.get())
        .filter(this::isTransitionEnabled)
        .findFirst()
        .ifPresent(this::releaseMutex);
  }

  private synchronized boolean isTransitionEnabled(MutexAndTransitions<T> mutexAndTransitions) {
    return actualState
        .findFirstTransitionEnabled(mutexAndTransitions.getTransition())
        .isPresent();
  }

  private synchronized void releaseMutex(MutexAndTransitions<T> mutexAndTransitions) {
    isActive.set(true);
    mutexAndTransitions
        .getMutex()
        .release();
  }
}
