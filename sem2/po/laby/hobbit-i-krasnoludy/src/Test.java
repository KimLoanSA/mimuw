
public class Test {
	public static void main(String[] args) {
		Hobbit Bilbo = new Hobbit();
		
		Skarb[] skarby = new Skarb[] {new Skarb(1), new Skarb(3), new Skarb(2), new Skarb(1), new Skarb(1), new Skarb(1)};
		Krasnolud[] krasnoludy = new Krasnolud[] {new KrasnoludLos(), new KrasnoludProcent(70), new KrasnoludStat()};
		Worek[] worki = new Worek[] {new Worek(3), new Worek(4), new Worek(2)};
		
		
		
		Bilbo.rozdziel(skarby, krasnoludy, worki);
	}
}
