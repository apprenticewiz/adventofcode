import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.regex.*;

public class Program {
    static final int ROW_MAX = 1000;
    static final int COL_MAX = 1000;

    public static void usage() {
        System.err.println("usage: java -jar Program.jar <input file>");
        System.exit(1);
    }

    public static void perform(int[] grid, String action, int r1, int c1, int r2, int c2) {
        int row, col;

        for ( row = r1; row <= r2; row++ ) {
            for ( col = c1; col <= c2; col++ ) {
                switch ( action ) {
                case "turn on":
                    grid[row*COL_MAX + col] += 1;
                    break;
                case "turn off":
                    grid[row*COL_MAX + col] = Integer.max(0, grid[row*COL_MAX + col] - 1);
                    break;
                case "toggle":
                    grid[row*COL_MAX + col] += 2;
                    break;
                default:
                    break;
                }
            }
        }
    }

    public static int sum(int[] grid) {
        int row, col;
        int total = 0;

        for ( row = 0; row < ROW_MAX; row++ ) {
            for ( col = 0; col < COL_MAX; col++ ) {
                total += grid[row*COL_MAX + col];
            }
        }
        return total;
    }

    public static int process(String filename) {
        Pattern p = Pattern.compile("(turn on|turn off|toggle) (\\d+),(\\d+) through (\\d+),(\\d+)");
       int[] grid = new int[ROW_MAX*COL_MAX];
        try ( BufferedReader reader = new BufferedReader(new FileReader(filename))) {
            String line;
            while ( (line = reader.readLine()) != null ) {
                Matcher m = p.matcher(line);
                if ( m.matches() ) {
                    String action = m.group(1);
                    int r1 = Integer.parseInt(m.group(2));
                    int c1 = Integer.parseInt(m.group(3));
                    int r2 = Integer.parseInt(m.group(4));
                    int c2 = Integer.parseInt(m.group(5));
                    perform(grid, action, r1, c1, r2, c2);
                }
            }
        } catch ( IOException e ) {
            System.err.println("Error reading file: " + e.getMessage());
            System.exit(1);
        }
        return sum(grid);
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
