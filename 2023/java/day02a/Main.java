import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class Main {
    static final int TOTAL_RED = 12;
    static final int TOTAL_GREEN = 13;
    static final int TOTAL_BLUE = 14;

    static void usage() {
        System.out.println("usage: java Main <file>");
        System.exit(1);
    }

    static int process(String filename) {
        int sum = 0;
        try {
            BufferedReader reader = new BufferedReader(new FileReader(filename));
            String line;
            while ( (line = reader.readLine()) != null ) {
                String[] lineSplit = line.split(": ");
                String gamePart = lineSplit[0];
                String drawsPart = lineSplit[1];
                String[] gameSplit = gamePart.split(" ");
                int gameNum = Integer.parseInt(gameSplit[1]);
                boolean valid = true;
                for ( String draw : drawsPart.split("; ") ) {
                    for ( String colorAmount : draw.split(", ") ) {
                        String[] colorAmountSplit = colorAmount.split(" ");
                        int amount = Integer.parseInt(colorAmountSplit[0]);
                        String color = colorAmountSplit[1];
                        if ( color.equals("red") && amount > TOTAL_RED ) {
                            valid = false;
                        }
                        else if ( color.equals("green") && amount > TOTAL_GREEN ) {
                            valid = false;
                        }
                        else if ( color.equals("blue") && amount > TOTAL_BLUE ) {
                            valid = false;
                        }
                    }
                }
                if ( valid ) {
                    sum += gameNum;
                }
            }
        } catch ( IOException e ) {
            e.printStackTrace();
        }
        return sum;
    }

    public static final void main(String[] args) {
        if ( args.length < 1 ) {
            usage();
        }
        String filename = args[0];
        int result = process(filename);
        System.out.println("result = " + Integer.toString(result));
    }
}
