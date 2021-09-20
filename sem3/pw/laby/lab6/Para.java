public class Para<T> {

  private T pierwszy;
  private T drugi;

  public Para(T pierwszy, T drugi) {
    this.pierwszy = pierwszy;
    this.drugi = drugi;
  }

  public synchronized void zamień() {
    T a = pierwszy;
    pierwszy = drugi;
    drugi = a;
  }

  public synchronized boolean sąRówne() {
    return pierwszy.equals(drugi);
  }

}
