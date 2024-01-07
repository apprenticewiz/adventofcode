import core.stdc.stdlib;
import std.conv;
import std.file;
import std.stdio;
import std.string;

void usage(string progname) {
    stderr.writef("usage: %s <file>\n", progname);
    exit(1);
}

uint process(string contents) {
    uint result = 0;

    foreach (line; contents.splitLines()) {
        auto game_reveals = line.split(": ");
        if (game_reveals.length == 2) {
            auto game_str = game_reveals[0];
            auto reveals_str = game_reveals[1];

            auto game_num_str = game_str.split(' ')[1];
            auto game_num = to!uint(game_num_str);

            uint red_needed = 0;
            uint green_needed = 0;
            uint blue_needed = 0;

            foreach (subset_str; reveals_str.split("; ")) {
                foreach (cubes_str; subset_str.split(", ")) {
                    auto cubes = cubes_str.split(' ');
                    if (cubes.length == 2) {
                        auto amount_str = cubes[0];
                        auto color = cubes[1];

                        auto amount = to!uint(amount_str);

                        switch (color) {
                            case "red":
                                if (amount > red_needed) {
                                    red_needed = amount;
                                }
                                break;
                            case "green":
                                if (amount > green_needed) {
                                    green_needed = amount;
                                }
                                break;
                            case "blue":
                                if (amount > blue_needed) {
                                    blue_needed = amount;
                                }
                                break;
                            default:
                                throw new Exception("unknown color");
                        }
                    }
                }
            }
            result += red_needed * green_needed * blue_needed;
        }
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
