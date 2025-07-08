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
		int totalArea = 0;
		try ( BufferedReader reader = new BufferedReader(new FileReader(filename))) {
			String line;
			while ( (line = reader.readLine()) != null ) {
				String[] numStrs = line.split("x");
				Vector<Integer> dimensions = new Vector<Integer>();
				for ( String numStr : numStrs ) {
					dimensions.addElement(Integer.parseInt(numStr));
				}
				int area1 = dimensions.elementAt(0) * dimensions.elementAt(1);
				int area2 = dimensions.elementAt(0) * dimensions.elementAt(2);
				int area3 = dimensions.elementAt(1) * dimensions.elementAt(2);
				int surfaceArea = area1 * 2 + area2 * 2 + area3 * 2;
				int minArea = Integer.min(Integer.min(area1, area2), area3);
				totalArea += surfaceArea + minArea;
			}
		} catch ( IOException e ) {
			System.err.println("Error reading file: " + e.getMessage());
			System.exit(1);
		}
		return totalArea;
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
