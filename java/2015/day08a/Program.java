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
                int memLen = 0;
                int i = 1;
                while ( i < codeLen - 1 ) {
                    switch ( line.charAt(i) ) {
                    case '\\':
                        switch ( line.charAt(i + 1) ) {
                        case '\\':
                        case '\"':
                            i += 2;
                            break;
                        case 'x':
                            i += 4;
                            break;
                        default:
                            i += 1;
                        }
                        break;
                    default:
                        i += 1;
                    }
                    memLen += 1;
                }
                result += codeLen - memLen;
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
