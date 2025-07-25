import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class Program {
    public static void usage() {
        System.err.println("usage: java -jar Program.jar <input file>");
        System.exit(1);
    }

    public static boolean prop1(String line) {
        for ( int i = 0; i < line.length() - 3; i++ ) {
            String first = line.substring(i, i + 2);
            for ( int j = i + 2; j < line.length() - 1; j++ ) {
                String second = line.substring(j, j + 2);
                if ( first.equals(second) ) {
                    return true;
                }
            }
        }
        return false;
    }

    public static boolean prop2(String line) {
        for ( int i = 0; i < line.length() - 2; i++ ) {
            String first = line.substring(i, i + 1);
            String second = line.substring(i + 2, i + 3);
            if ( first.equals(second) ) {
                return true;
            }
        }
        return false;
    }

    public static int process(String filename) {
        int result = 0;
        try ( BufferedReader reader = new BufferedReader(new FileReader(filename))) {
            String line;
            while ( (line = reader.readLine()) != null ) {
                if ( prop1(line) && prop2(line) ) {
                    result++;
                }
            }
        } catch ( IOException e ) {
            System.err.println("Error reading file: " + e.getMessage());
            System.exit(1);
        }
        return result;
    }

    public static final void main(String[] args) {
        if ( args.length < 1 ) {
            usage();
        }
        String filename = args[0];
        int result = process(filename);
        System.out.println("result = " + result);
    }
}
