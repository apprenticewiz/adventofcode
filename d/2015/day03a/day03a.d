import std.conv;
import std.exception;
import std.file;
import std.stdio;

import aoc_utils.geometry;

void usage(string progname) {
    stderr.writeln("usage: ", progname, " <input file>");
}

int process(string filename) {
    Position2D!int santa = Position2D!int(0, 0);
    bool[string] positions;

    foreach ( line; File(filename).byLine() ) {
        foreach ( ch; line ) {
            switch ( ch ) {
                case '^':
                    santa.y += 1;
                    break;
                case 'v':
                    santa.y -= 1;
                    break;
                case '<':
                    santa.x -= 1;
                    break;
                case '>':
                    santa.x += 1;
                    break;
                default:
                    break;
            }
            string position = text(santa.x, ",", santa.y);
            positions[position] = true;
        }
    }

  return to!int(positions.length);
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
