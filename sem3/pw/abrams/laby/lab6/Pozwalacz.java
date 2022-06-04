import java.util.ArrayList;
import java.util.List;

public class Pozwalacz<T> {

  private T wartość;
  private final List<T> historia = new ArrayList<>();

  public Pozwalacz(T wartość) {
    this.wartość = wartość;
    historia.add(wartość);
  }

  public synchronized void pozwól(T stara, T nowa) throws InterruptedException {
    while (!wartość.equals(stara)) {
      wait();
    }
    wartość = nowa;
    historia.add(nowa);
    notifyAll();
  }

  public synchronized List<T> dajHistorię() {
    return historia;
  }

}
