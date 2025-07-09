import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

public class Program {
	public static void usage() {
		System.err.println("usage: java -jar Program.jar <key>");
		System.exit(1);
	}

	public static int process(String key) throws NoSuchAlgorithmException {
		MessageDigest md5 = MessageDigest.getInstance("MD5");
		int n = 1;
		while ( true ) {
			String tryKey = key + String.format("%d", n);
			byte[] digestBytes = md5.digest(tryKey.getBytes());
			StringBuffer digestChars = new StringBuffer();
			for ( byte b : digestBytes ) {
				digestChars.append(String.format("%02x", b));
			}
			String digest = digestChars.toString();
			if ( digest.startsWith("00000") ) {
				break;
			} else {
				n += 1;
			}
		}
		return n;
	}

	public static final void main(String[] args) {
		if ( args.length < 1 ) {
			usage();
		}
		String key = args[0];
		int result = 0;
		try {
			result = process(key);
		} catch ( NoSuchAlgorithmException e ) {
			System.err.println("could not find md5 algorithm:" + e.toString());
			System.exit(1);
		}
		System.out.println("result = " + result);
	}
}
