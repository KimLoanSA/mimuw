import java.io.IOException;

public class Main {
    public static void main(String[] args) {
        if (args.length != 2) {
            System.err.println("Wrong number of arguments (given " + args.length + ", expected 2)");
            System.exit(-1);
        }

        String dir = args[0];
        String zipDir = args[1];

        try {
            new WatchDir(dir, zipDir, true).processEvents();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
