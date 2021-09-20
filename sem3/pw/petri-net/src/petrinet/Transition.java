package petrinet;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class Transition<T> {

  private Map<T, Integer> input;
  private List<T> reset;
  private List<T> inhibitor;
  private Map<T, Integer> output;

  public Transition(Map<T, Integer> input, Collection<T> reset, Collection<T> inhibitor, Map<T, Integer> output) {
    this.input = new ConcurrentHashMap<>(input);
    this.reset = new ArrayList<>(reset);
    this.inhibitor = new ArrayList<>(inhibitor);
    this.output = new ConcurrentHashMap<>(output);
  }

  public Map<T, Integer> getInput() {
    return input;
  }

  public List<T> getReset() {
    return reset;
  }

  public List<T> getInhibitor() {
    return inhibitor;
  }

  public Map<T, Integer> getOutput() {
    return output;
  }

}

