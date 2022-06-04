package petrinet.models;

import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import petrinet.Transition;

public class State<T> {

  private Map<T, Integer> tokens;

  public State(Map<T, Integer> tokens) {
    this.tokens = new ConcurrentHashMap<>(tokens);
  }

  public State(State<T> state) {
    this.tokens = new ConcurrentHashMap<>(state.tokens);
  }

  public Map<T, Integer> getMap() {
    return tokens;
  }

  public Optional<Transition<T>> findFirstTransitionEnabled(List<Transition<T>> transitions) {
    return transitions
        .stream()
        .filter(this::isTransitionEnabled)
        .findFirst();
  }
  public boolean isTransitionEnabled(Transition<T> transition) {
    return validateInhibitorArcs(transition)
        && validateInputArcs(transition);
  }

  private boolean validateInhibitorArcs(Transition<T> transition) {
    return transition
        .getInhibitor()
        .stream()
        .allMatch(this::validateInhibitorArc);
  }

  private boolean validateInhibitorArc(T place) {
    return getTokensOfPlace(place) == 0;
  }

  private boolean validateInputArcs(Transition<T> transition) {
    return transition
        .getInput()
        .entrySet()
        .stream()
        .allMatch(this::validateInputArc);
  }

  private boolean validateInputArc(Entry<T, Integer> arc) {
    return getTokensOfPlace(arc.getKey()) >= arc.getValue();
  }

  private Integer getTokensOfPlace(T place) {
    return tokens.getOrDefault(place, 0);
  }
}
