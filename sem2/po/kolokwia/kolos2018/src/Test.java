
public class Test {
	public static void main(String[] args) {
		Funkcja f = new FunTab(new int[] {1,2,3}, new int[] {1,2,2});
		Funkcja g = new FunTab(new int[] {1,2,3}, new int[] {3,1,1});
		
		HammingDistance metrykaH = new HammingDistance();
		EarthMover metrykaE = new EarthMover();
		
		System.out.println(metrykaH.odleglosc(f,g));
		System.out.println(metrykaE.odleglosc(f,g));
	}
}
