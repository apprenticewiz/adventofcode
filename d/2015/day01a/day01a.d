import std.exception;
import std.file;
import std.stdio;

void usage(string progname) {
  stderr.writeln("usage: ", progname, " <input file>");
}

int process(string filename) {
  int floors = 0;

  foreach ( line; File(filename).byLine() ) {
      foreach ( ch; line ) {
          switch ( ch ) {
              case '(':
                  floors += 1;
                  break;
              case ')':
                  floors -= 1;
                  break;
            default:
                  break;
          }
      }
  }

  return floors;
}

int main(string[] args) {
    string progname = args[0];

    if ( args.length < 2 ) {
        usage(progname);
	return 1;
    }

    string filename = args[1];

    try {
        int result = process(filename);
        writeln("result = ", result);
    } catch ( Exception e ) {
        stderr.writeln("error while processing file `", filename, "`: `", e.msg);
	return 1;
    }

    return 0;
}
