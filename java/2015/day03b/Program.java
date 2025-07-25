import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashSet;

import aoc_utils.geometry.Position2D;

public class Program {
    public static void usage() {
        System.err.println("usage: java -jar Program.jar <input file>");
        System.exit(1);
    }

    public static int process(String filename) {
        HashSet<Position2D<Integer>> positions = new HashSet<Position2D<Integer>>();
        try ( BufferedReader reader = new BufferedReader(new FileReader(filename))) {
            String line;
            Position2D<Integer> santa = new Position2D<Integer>(0, 0);
            Position2D<Integer> roboSanta = new Position2D<Integer>(0, 0);
            boolean santaMoves = true;
            while ( (line = reader.readLine()) != null ) {
                for ( char ch : line.toCharArray() ) {
                    switch ( ch ) {
                        case '^':
                            if ( santaMoves ) {
                                santa.y += 1;
                            } else {
                                roboSanta.y += 1;
                            }
                            break;
                        case 'v':
                            if ( santaMoves ) {
                                santa.y -= 1;
                            } else {
                                roboSanta.y -= 1;
                            }
                            break;
                        case '<':
                            if ( santaMoves ) {
                                santa.x -= 1;
                            } else {
                                roboSanta.x -= 1;
                            }
                            break;
                        case '>':
                            if ( santaMoves ) {
                                santa.x += 1;
                            } else {
                                roboSanta.x += 1;
                            }
                            break;
                        default:
                            break;
                    }
                    if ( santaMoves ) {
                        positions.add(new Position2D<>(santa));
                    } else {
                        positions.add(new Position2D<>(roboSanta));
                    }
                    santaMoves = !santaMoves;
                }
            }
        } catch ( IOException e ) {
            System.err.println("Error reading file: " + e.getMessage());
            System.exit(1);
        }
        return positions.size();
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
