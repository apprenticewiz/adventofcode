import core.stdc.stdlib;
import std.conv;
import std.file;
import std.regex;
import std.stdio;
import std.string;

void usage(string progname) {
    stderr.writef("usage: %s <file>\n", progname);
    exit(1);
}

uint process(string contents) {
    uint result = 0;
    auto line_re = regex(r":\s+");
    auto numsplit_re = regex(r"\s+\|\s+");
    auto ws_re = regex(r"\s+");
    foreach (line; contents.splitLines()) {
        string rest = split(line, line_re)[1];
        string winning_str = split(rest, numsplit_re)[0];
        bool[int] winning_set;
        foreach (winning_num; split(winning_str, ws_re)) {
            winning_set[to!int(winning_num)] = true;
        }
        string hand_str = split(rest, numsplit_re)[1];
        bool[int] hand_set;
        foreach (hand_num; split(hand_str, ws_re)) {
            hand_set[to!int(hand_num)] = true;
        }
        bool[int] intersection;
        foreach (winning_num; winning_set.keys) {
            if (winning_num in hand_set) {
                intersection[winning_num] = true;
            }
        }
        result += (intersection.length > 0) ? 1 << (intersection.length - 1) : 0;
    }
    return result;
}

void main(string[] args) {
    if ( args.length < 2 ) {
        usage(args[0]);
    }
    string filename = args[1];
    string contents = readText(filename);
    int result = process(contents);
    writef("result = %d\n", result);
}
