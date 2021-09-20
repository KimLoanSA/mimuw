package petrinet;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;
import petrinet.models.MutexAndTransitions;
import petrinet.models.State;
import petrinet.resolvers.PetriNetFireResolver;
import petrinet.resolvers.ReachableStatesResolver;
import petrinet.resolvers.TransitionMutexResolver;

public class PetriNet<T> {

  private State<T> placesTokens;
  private TransitionMutexResolver<T> mutexResolver;

  public PetriNet(Map<T, Integer> initial, boolean fair) {
    this.placesTokens = new State<>(initial);
    this.mutexResolver =  new TransitionMutexResolver<>(placesTokens, fair);
  }

  public Set<Map<T, Integer>> reachable(Collection<Transition<T>> transitions) {
    ReachableStatesResolver<T> resolver = new ReachableStatesResolver<>(placesTokens, new ArrayList<>(transitions));

    return resolver.findAll();
  }

  public Transition<T> fire(Collection<Transition<T>> transitions) throws InterruptedException {
    List<Transition<T>> transitionsList = new ArrayList<>(transitions);
    MutexAndTransitions<T> actualMutexAndTransitions = mutexResolver.getMutex(transitionsList);

    try {
      actualMutexAndTransitions
          .getMutex()
          .acquire();

    } catch (InterruptedException e) {
      mutexResolver.removeFromQueue(actualMutexAndTransitions);

      throw e;
    }

    return fireReleaseAndSave(transitionsList, actualMutexAndTransitions);
  }

  private Transition<T> fireReleaseAndSave(List<Transition<T>> transitionsList,
      MutexAndTransitions<T> actualMutexAndTransitions) {

    PetriNetFireResolver<T> fireResolver = new PetriNetFireResolver<>(placesTokens);

    Transition<T> transitionToFire = placesTokens
        .findFirstTransitionEnabled(transitionsList)
        .get(); // zawsze sie znajdzie bo dodaje do kolejki
    placesTokens = fireResolver.fire(transitionToFire);

    cleanMutex(actualMutexAndTransitions);

    return transitionToFire;
  }

  private void cleanMutex(MutexAndTransitions<T> actualMutexAndTransitions) {
    mutexResolver.removeFromQueue(actualMutexAndTransitions);
    mutexResolver.updateStateAndReleaseNext(placesTokens);
  }

}
