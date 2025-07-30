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
    int totalArea = 0;

  foreach ( line; File(filename).byLine() ) {
        auto dims = split(line, regex(`x`));
        int l = to!int(dims[0]);
        int w = to!int(dims[1]);
        int h = to!int(dims[2]);
        int area1 = l * w;
        int area2 = l * h;
        int area3 = w * h;
        int surfaceArea = 2 * area1 + 2 * area2 + 2 * area3;
        int minArea = [area1, area2, area3].minElement;
        totalArea += surfaceArea + minArea;
    }

  return totalArea;
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
