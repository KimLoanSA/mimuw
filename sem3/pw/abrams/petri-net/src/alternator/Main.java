package alternator;

public class Main {

  private final static Long SLEEP_TIME = 30000L;
  private final static Boolean FAIR = true;

  public static void main(String[] args) {
    new Alternator(SLEEP_TIME, FAIR).run();
  }

}
