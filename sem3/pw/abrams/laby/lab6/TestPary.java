public class TestPary {

  private static final int ZAMIANY = 1000000;
  private static final int SPRAWDZENIA = 1000000;

  public static void main(String[] args) {
    Para<Integer> para = new Para<>(1, 2);
    new Thread(() -> {
      for (int i = 0; i < ZAMIANY; ++i) {
        para.zamień();
      }

    }).start();
    int równe = 0;
    for (int i = 0; i < SPRAWDZENIA; ++i) {
      if (para.sąRówne()) {
        ++równe;
      }
    }
    System.out.println("Równe: " + równe);
  }

}
