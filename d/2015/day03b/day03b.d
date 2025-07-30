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
    Position2D!int roboSanta = Position2D!int(0, 0);
    bool santaMove = true;
    bool[string] positions;

    foreach ( line; File(filename).byLine() ) {
        foreach ( ch; line ) {
            switch ( ch ) {
                case '^':
                    santaMove ? (santa.y += 1) : (roboSanta.y += 1);
                    break;
                case 'v':
                    santaMove ? (santa.y -= 1) : (roboSanta.y -= 1);
                    break;
                case '<':
                    santaMove ? (santa.x -= 1) : (roboSanta.x -= 1);
                    break;
                case '>':
                    santaMove ? (santa.x += 1) : (roboSanta.x += 1);
                    break;
                default:
                    break;
            }
            string position = santaMove ? text(santa.x, ",", santa.y) :
                text(roboSanta.x, ",", roboSanta.y);
            positions[position] = true;
            santaMove = !santaMove;
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
