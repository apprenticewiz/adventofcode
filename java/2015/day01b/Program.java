import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class Program {
    public static void usage() {
        System.err.println("usage: java -jar Program.jar <input file>");
        System.exit(1);
    }

    public static int process(String filename) {
        int counter = 0;
        int pos = 1;
        try ( BufferedReader reader = new BufferedReader(new FileReader(filename))) {
            String line;
            while ( (line = reader.readLine()) != null ) {
                for ( char ch : line.toCharArray() ) {
                    switch ( ch ) {
                        case '(':
                            counter++;
                            break;
                        case ')':
                            counter--;
                            break;
                    }
                    if ( counter < 0 ) {
                        return pos;
                    } else {
                        pos++;
                    }
                }
            }
        } catch ( IOException e ) {
            System.err.println("Error reading file: " + e.getMessage());
            System.exit(1);
        }
        return 0;
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
