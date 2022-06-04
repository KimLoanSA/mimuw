package petrinet.resolvers;

import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Queue;
import java.util.Set;
import petrinet.Transition;
import petrinet.models.State;

public class ReachableStatesResolver<T> {

  private State<T> startingState;
  private List<Transition<T>> transitions;
  private Set<Map<T, Integer>> reachableStates;
  private Queue<State<T>> bfsQueue = new LinkedList<>();

  public ReachableStatesResolver(State<T> startingState, List<Transition<T>> transitions) {
    this.startingState = new State<>(startingState);
    this.transitions = transitions;
    this.reachableStates = new HashSet<>();
  }

  public Set<Map<T, Integer>> findAll() {
    bfsQueue.add(startingState);
    findAllReachable();

    return reachableStates;
  }

  private void findAllReachable() {
    Optional.ofNullable(bfsQueue.poll())
        .filter(state -> reachableStates.add(state.getMap()))
        .ifPresent(this::findReachableForState);
  }

  private void findReachableForState(State<T> actualState) {
    transitions.stream()
        .filter(actualState::isTransitionEnabled)
        .forEach(transition -> addToQueue(calculateNewState(actualState, transition)));

    findAllReachable();
  }

  private void addToQueue(State<T> actualState) {
    bfsQueue.add(actualState);
  }

  private State<T> calculateNewState(State<T> actualState, Transition<T> transition) {
    PetriNetFireResolver<T> resolver = new PetriNetFireResolver<>(actualState);

    return resolver.fire(transition);
  }

}
