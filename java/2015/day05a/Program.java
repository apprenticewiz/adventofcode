import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.regex.*;

public class Program {
    public static void usage() {
        System.err.println("usage: java -jar Program.jar <input file>");
        System.exit(1);
    }

    public static boolean prop1(String line) {
        int vowels = 0;
        Pattern vowelPattern = Pattern.compile("[aeiou]");
        Matcher matcher = vowelPattern.matcher(line);
        while ( matcher.find() ) {
            vowels++;
        }
        return vowels >= 3;
    }

    public static boolean prop2(String line) {
        Pattern p = Pattern.compile("(.)\\1");
        Matcher m = p.matcher(line);
        return m.find();
    }

    public static boolean prop3(String line) {
        return !line.contains("ab") && !line.contains("cd") &&
            !line.contains("pq") && !line.contains("xy");
    }

    public static int process(String filename) {
        int result = 0;
        try ( BufferedReader reader = new BufferedReader(new FileReader(filename))) {
            String line;
            while ( (line = reader.readLine()) != null ) {
                if ( prop1(line) && prop2(line) && prop3(line) ) {
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
