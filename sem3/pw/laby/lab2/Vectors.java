import java.util.ArrayList;
import java.util.List;
import java.util.Random;

public class Vectors {

  private static volatile List<Integer> suma;
  private static volatile int liczbaSuma = 0;
  private static volatile int liczbaIloczyn = 0;
  private static volatile int iloczyn;

  private static final Random rand = new Random();
  private static final int dlugoscPrzedzialu = 10;

  private static class Dodajacy implements Runnable {

    private final List<Integer> vector1;
    private final List<Integer> vector2;
    private final int indeks;

    public Dodajacy(List<Integer> vector1, List<Integer> vector2, int indeks) {
      this.vector1 = vector1;
      this.vector2 = vector2;
      this.indeks = indeks;
    }

    @Override
    public void run() {
      for (int i = 0; i < vector1.size(); i++) {
        suma.add(indeks + i, vector1.get(i) + vector2.get(i));
      }

      liczbaSuma++;
    }
  }


  private static class Iloczynujacy implements Runnable {

    private final List<Integer> vector1;
    private final List<Integer> vector2;

    public Iloczynujacy(List<Integer> vector1, List<Integer> vector2) {
      this.vector1 = vector1;
      this.vector2 = vector2;
    }

    @Override
    public void run() {
      for (int i = 0; i < vector1.size(); i++) {
        iloczyn += vector1.get(i) * vector2.get(i);
      }

      liczbaIloczyn++;
    }


  }


  private static List<Integer> generateVector(int n) {
    List<Integer> res = new ArrayList<>();

    for (int i = 0; i < n; i++) {
      res.add(rand.nextInt());
    }

    return res;
  }

  private static List<Integer> suma(List<Integer> vector1, List<Integer> vector2) {
    List<Integer> res = new ArrayList<>();

    for (int i = 0; i < vector1.size(); i++) {
      res.add(vector1.get(i) + vector2.get(i));
    }

    return res;
  }

  private static int iloczyn(List<Integer> vector1, List<Integer> vector2) {
    int res = 0;

    for (int i = 0; i < vector1.size(); i++) {
      res += vector1.get(i) * vector2.get(i);
    }

    return res;
  }

  private static boolean vectorEqual(List<Integer> v1, List<Integer> v2) {
    if (v1.size() != v2.size()) {
      return false;
    }

    for (int i = 0; i < v1.size(); i++) {
      if (!v1.get(i).equals(v2.get(i))) {
        return false;
      }
    }

    return true;
  }

  private static void policzSume(List<Integer> vector1, List<Integer> vector2) {
    suma = new ArrayList<>();
    liczbaSuma = 0;

    for (int i = 0; i < vector1.size(); i += dlugoscPrzedzialu) {
      Thread thread = new Thread(
          new Dodajacy(vector1.subList(i, Math.min(i + dlugoscPrzedzialu, vector1.size())),
              vector2.subList(i, Math.min(i + dlugoscPrzedzialu, vector2.size())), i));

      try {
        thread.start();
        thread.join();
      } catch (Exception e) {
        System.out.println("SPERMA1");
      }

    }

    while (liczbaSuma != vector1.size() / dlugoscPrzedzialu + 1) {
      Thread.yield();
    }
  }

  private static void policzIloczyn(List<Integer> vector1, List<Integer> vector2) {
    iloczyn = 0;
    liczbaIloczyn = 0;

    for (int i = 0; i < vector1.size(); i += dlugoscPrzedzialu) {
      Thread thread = new Thread(
          new Iloczynujacy(vector1.subList(i, Math.min(i + dlugoscPrzedzialu, vector1.size())),
              vector2.subList(i, Math.min(i + dlugoscPrzedzialu, vector2.size()))));

      try {
        thread.start();
        thread.join();
      } catch (Exception e) {
        System.out.println("SPERMA2");
      }

    }

    while (liczbaIloczyn != vector1.size() / dlugoscPrzedzialu + 1) {
      Thread.yield();
    }
  }

  public static void main(String args[]) {

    int n = 75;
    List<Integer> v1 = generateVector(n);
    List<Integer> v2 = generateVector(n);

    policzSume(v1, v2);
    policzIloczyn(v1, v2);

    System.out.println("suma: " + vectorEqual(suma(v1, v2), suma));
    System.out.println("iloczyn: " + (iloczyn(v1, v2) == iloczyn));
  }
}
