import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.regex.*;

public class Program {
    public static void usage() {
        System.err.println("usage: java -jar Program.jar <input file>");
        System.exit(1);
    }

    public static int process(String filename) {
        int result = 0;
        try ( BufferedReader reader = new BufferedReader(new FileReader(filename))) {
            String line;
            while ( (line = reader.readLine()) != null ) {
                int codeLen = line.length();
                int encLen = 0;
                int i = 0;
                for ( i = 0; i < codeLen; i++ ) {
                    switch ( line.charAt(i) ) {
                    case '\\':
                    case '\"':
                        encLen += 2;
                        break;
                    default:
                        encLen += 1;
                    }
                }
                result += 2 + (encLen - codeLen);
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
