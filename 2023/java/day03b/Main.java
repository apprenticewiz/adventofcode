import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

public class Main {
    static class Position {
        public int row;
        public int col;

        Position(int row, int col) {
            this.row = row;
            this.col = col;
        }
    }

    static void usage() {
        System.out.println("usage: java Main <file>");
        System.exit(1);
    }

    static HashMap<Position, String> buildNumbers(String filename) {
        HashMap<Position, String> numberLocs = new HashMap<Position, String>();
        try {
            BufferedReader reader = new BufferedReader(new FileReader(filename));
            String line;
            int row = 0;
            boolean scanningNumber = false;
            StringBuffer number = new StringBuffer();
            Position currentPos = new Position(-1, -1);
            while ( (line = reader.readLine()) != null ) {
                for ( int col = 0; col < line.length(); col++ ) {
                    char ch = line.charAt(col);
                    if ( scanningNumber ) {
                        if ( Character.isDigit(ch) ) {
                            number.append(ch);
                        } else {
                            numberLocs.put(currentPos, number.toString());
                            scanningNumber = false;
                            number = new StringBuffer();
                        }
                    } else {
                        if ( Character.isDigit(ch) ) {
                            number.append(ch);
                            currentPos = new Position(row, col);
                            scanningNumber = true;
                        }
                    }
                }
                if ( scanningNumber ) {
                    numberLocs.put(currentPos, number.toString());
                    scanningNumber = false;
                    number = new StringBuffer();
                }
                row++;
            }
            reader.close();
        } catch ( IOException e ) {
            e.printStackTrace();
        }
        return numberLocs;
    }

    static HashMap<Position, Character> buildParts(String filename) {
        HashMap<Position, Character> partLocs = new HashMap<Position, Character>();
        try {
            BufferedReader reader = new BufferedReader(new FileReader(filename));
            String line;
            int row = 0;
            while ( (line = reader.readLine()) != null ) {
                for ( int col = 0; col < line.length(); col++ ) {
                    char ch = line.charAt(col);
                    if ( ch == '*' ) {
                        partLocs.put(new Position(row, col), ch);
                    }
                }
                row++;
            }
            reader.close();
        } catch ( IOException e ) {
            e.printStackTrace();
        }
        return partLocs;
    }

    static int checkParts(HashMap<Position, String> numLocs, HashMap<Position, Character> partLocs) {
        int result = 0;
        ArrayList<Position> neighbors = new ArrayList<Position>();
        for ( int i = -1; i <= 1; i++ ) {
            for ( int j = -1; j <= 1; j++ ) {
                if ( !( i == j && i == 0) ) {
                    neighbors.add(new Position(i, j));
                }
            }
        }
        for ( Map.Entry<Position, Character> partLocEntry : partLocs.entrySet() ) {
            Position partLoc = partLocEntry.getKey();
            int adjacentCount = 0;
            int prod = 1;
            for ( Map.Entry<Position, String> numLocEntry : numLocs.entrySet() ) {
                Position numLoc = numLocEntry.getKey();
                String number = numLocEntry.getValue();
                int numRow = numLoc.row;
                int numColFirst = numLoc.col;
                int numColLast = numLoc.col + number.length();
                boolean found = false;
                for ( Position neighbor : neighbors ) {
                    Position adjacent = new Position(partLoc.row + neighbor.row, partLoc.col + neighbor.col);
                    for ( int numCol = numColFirst; numCol < numColLast; numCol++ ) {
                        if ( numRow == adjacent.row && numCol == adjacent.col ) {
                            adjacentCount++;
                            prod *= Integer.parseInt(number);
                            found = true;
                            break;
                        }
                    }
                    if ( found ) {
                        break;
                    }
                }
            }
            if ( adjacentCount == 2 ) {
                result += prod;
            }
        }
        return result;
    }

    static int process(String filename) {
        HashMap<Position, String> numLocs = buildNumbers(filename);
        HashMap<Position, Character> partLocs = buildParts(filename);
        return checkParts(numLocs, partLocs);
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
