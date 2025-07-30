import std.algorithm;
import std.conv;
import std.exception;
import std.file;
import std.regex;
import std.stdio;

void usage(string progname) {
  stderr.writeln("usage: ", progname, " <input file>");
}

int process(string filename) {
  int totalLen = 0;

  foreach ( line; File(filename).byLine() ) {
    auto dims = split(line, regex(`x`));
    int l = to!int(dims[0]);
    int w = to!int(dims[1]);
    int h = to!int(dims[2]);
    int perim1 = 2 * (l + w);
    int perim2 = 2 * (l + h);
    int perim3 = 2 * (w + h);
    int presentLen = [perim1, perim2, perim3].minElement;
    int bowLen = l * w * h;
    totalLen += presentLen + bowLen;
  }

  return totalLen;
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
        stderr.writeln("error while processing file `", filename, "`: ", e.msg);
        return 1;
    }

    return 0;
}
