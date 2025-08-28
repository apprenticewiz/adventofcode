import std.algorithm;
import std.array;
import std.conv;
import std.exception;
import std.file;
import std.regex;
import std.stdio;

enum ROW_MAX = 1000;
enum COL_MAX = 1000;

void usage(string progname) {
    stderr.writeln("usage: ", progname, " <input file>");
}

void perform(int[] grid, string action, int r1, int c1, int r2, int c2) {
    for ( auto row = r1; row <= r2; row++ ) {
        for ( auto col = c1; col <= c2; col++ ) {
            if ( action == "turn on" ) {
                grid[row*COL_MAX + col] += 1;
            } else if ( action == "turn off" ) {
                grid[row*COL_MAX + col] = max(0, grid[row*COL_MAX + col] - 1);
            } else if ( action == "toggle" ) {
                grid[row*COL_MAX + col] += 2;
           }
        }
    }
}

int sum(int[] grid) {
    int total = 0;
    for ( auto row = 0; row < ROW_MAX; row++ ) {
        for ( auto col = 0; col < COL_MAX; col++ ) {
            total += grid[row*COL_MAX + col];
        }
    }
    return total;
}

int process(string filename) {
    int[ROW_MAX*COL_MAX] grid;
    foreach ( line; File(filename).byLine() ) {
        auto re = regex(`(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)`);
        auto caps = matchFirst(line, re);
        string action = to!string(caps[1]);
        int r1 = to!int(caps[2]);
        int c1 = to!int(caps[3]);
        int r2 = to!int(caps[4]);
        int c2 = to!int(caps[5]);
        perform(grid, action, r1, c1, r2, c2);
    }
    return sum(grid);
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
