import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;

public class Main {
    static void usage() {
        System.out.println("usage: java Main <file>");
        System.exit(1);
    }

    static int process(String filename) {
        HashMap<Integer, Integer> instances = new HashMap<Integer, Integer>();
        int result = 0;
        try {
            BufferedReader reader = new BufferedReader(new FileReader(filename));
            String line;
            while ( (line = reader.readLine()) != null ) {
                String cardPart = line.split(":\\s+")[0];
                int cardNumber = Integer.parseInt(cardPart.split("\\s+")[1]);
                String rest = line.split(":\\s+")[1];
                String winningStr = rest.split("\\s+\\|\\s+")[0];
                HashSet<Integer> winningSet = new HashSet<Integer>();
                for ( String winningNum : winningStr.split("\\s+") ) {
                    winningSet.add(Integer.parseInt(winningNum));
                }
                String handStr = rest.split("\\s+\\|\\s+")[1];
                HashSet<Integer> handSet = new HashSet<Integer>();
                for ( String handNum : handStr.split("\\s+") ) {
                    handSet.add(Integer.parseInt(handNum));
                }
                HashSet<Integer> intersection = new HashSet<Integer>(winningSet);
                intersection.retainAll(handSet);
                for ( int i = cardNumber + 1; i < cardNumber + intersection.size() + 1; i++ ) {
                    int copies = 0;
                    if ( instances.containsKey(i) ) {
                        copies += instances.get(i);
                    }
                    copies += 1;
                    if ( instances.containsKey(cardNumber) ) {
                        copies += instances.get(cardNumber);
                    }
                    instances.put(i, copies);
                }
                result += 1;
            }
            reader.close();
        } catch ( IOException e ) {
            e.printStackTrace();
        }
        for ( int i : instances.keySet() ) {
            result += instances.get(i);
        }
        return result;
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
