import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.Vector;

public class Program {
    public static void usage() {
        System.err.println("usage: java -jar Program.jar <input file>");
        System.exit(1);
    }

    public static int process(String filename) {
        int totalLen = 0;
        try ( BufferedReader reader = new BufferedReader(new FileReader(filename))) {
            String line;
            while ( (line = reader.readLine()) != null ) {
                String[] numStrs = line.split("x");
                Vector<Integer> dimensions = new Vector<Integer>();
                for ( String numStr : numStrs ) {
                    dimensions.addElement(Integer.parseInt(numStr));
                }
                int perim1 = (dimensions.elementAt(0) + dimensions.elementAt(1)) * 2;
                int perim2 = (dimensions.elementAt(0) + dimensions.elementAt(2)) * 2;
                int perim3 = (dimensions.elementAt(1) + dimensions.elementAt(2)) * 2;
                int presentLen = Integer.min(Integer.min(perim1, perim2), perim3);
                int bowLen = dimensions.elementAt(0) * dimensions.elementAt(1) * dimensions.elementAt(2);
                totalLen += presentLen + bowLen;
            }
        } catch ( IOException e ) {
            System.err.println("Error reading file: " + e.getMessage());
            System.exit(1);
        }
        return totalLen;
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
