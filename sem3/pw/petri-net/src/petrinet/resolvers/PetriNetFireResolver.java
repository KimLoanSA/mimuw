package petrinet.resolvers;

import java.util.Map.Entry;
import petrinet.Transition;
import petrinet.models.State;

public class PetriNetFireResolver<T> {

  private State<T> state;

  public PetriNetFireResolver(State<T> state) {
    this.state = new State<>(state);
  }

  public State<T> fire(Transition<T> transition) {
    applyInputArcs(transition);
    applyResetArcs(transition);
    applyOutputArcs(transition);

    return state;
  }

  private void applyInputArcs(Transition<T> transition) {
    transition
        .getInput()
        .entrySet()
        .forEach(this::applyInputArc);
  }

  private void applyInputArc(Entry<T, Integer> arc) {
    state
        .getMap()
        .computeIfPresent(arc.getKey(), (k, v) -> substractOrRemove(v, arc.getValue()));
  }

  private Integer substractOrRemove(Integer value, Integer arcWeight) {
    return value.equals(arcWeight) ? null : value - arcWeight;
  }

  private void applyResetArcs(Transition<T> transition) {
    transition.getReset()
        .forEach(this::applyResetArc);
  }

  private void applyResetArc(T place) {
    state
        .getMap()
        .remove(place);
  }

  private void applyOutputArcs(Transition<T> transition) {
    transition
        .getOutput()
        .entrySet()
        .forEach(this::applyOutputArc);
  }

  private void applyOutputArc(Entry<T, Integer> arc) {
    state
        .getMap()
        .putIfAbsent(arc.getKey(), 0);

    state
        .getMap()
        .computeIfPresent(arc.getKey(), (k, v) -> v + arc.getValue());
  }

}
