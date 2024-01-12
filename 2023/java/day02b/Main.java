import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class Main {
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
                int redNeeded = 0;
                int greenNeeded = 0;
                int blueNeeded = 0;
                for ( String draw : drawsPart.split("; ") ) {
                    for ( String colorAmount : draw.split(", ") ) {
                        String[] colorAmountSplit = colorAmount.split(" ");
                        int amount = Integer.parseInt(colorAmountSplit[0]);
                        String color = colorAmountSplit[1];
                        if ( color.equals("red") && amount > redNeeded ) {
                            redNeeded = amount;
                        }
                        else if ( color.equals("green") && amount > greenNeeded ) {
                            greenNeeded = amount;
                        }
                        else if ( color.equals("blue") && amount > blueNeeded ) {
                            blueNeeded = amount;
                        }
                    }
                }
                sum += redNeeded * greenNeeded * blueNeeded;
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
