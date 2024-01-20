import core.stdc.stdlib;
import std.ascii;
import std.conv;
import std.file;
import std.stdio;
import std.string;
import std.typecons;

void usage(string progname) {
    stderr.writef("usage: %s <file>\n", progname);
    exit(1);
}

string[Tuple!(int, int)] build_numbers(string contents) {
    string[Tuple!(int, int)] number_locs;
    bool scanning_number = false;
    string number = "";
    Tuple!(int, int) current_pos;
    foreach ( size_t row, string line; contents.splitLines() ) {
        foreach ( size_t col, char ch; line ) {
            if ( scanning_number ) {
                if ( isDigit(ch) ) {
                    number ~= ch;
                } else {
                    number_locs[current_pos] = number;
                    scanning_number = false;
                    number = "";
                }
            } else {
                if ( isDigit(ch) ) {
                    current_pos = tuple(to!int(row), to!int(col));
                    number ~= ch;
                    scanning_number = true;
                }
            }
        }
        if ( scanning_number ) {
            number_locs[current_pos] = number;
            scanning_number = false;
            number = "";
        }
    }
    return number_locs;
}

char[Tuple!(int, int)] build_gears(string contents) {
    char[Tuple!(int, int)] gear_locs;
    foreach ( size_t row, string line; contents.splitLines() ) {
        foreach ( size_t col, char ch; line ) {
            if ( !isDigit(ch) && ch != '.' ) {
                gear_locs[tuple(to!int(row), to!int(col))] = ch;
            }
        }
    }
    return gear_locs;
}

uint check_gears(string[Tuple!(int, int)] number_locs, char[Tuple!(int, int)] gear_locs) {
    uint result = 0;
    foreach ( Tuple!(int, int) gear_loc; gear_locs.keys ) {
        uint[] adjacents = [];
        foreach ( Tuple!(int, int) num_loc; number_locs.keys ) {
            bool skip = false;
            for ( int delta_row = -1; delta_row <= 1; delta_row++ ) {
                int adjacent_row = gear_loc[0] + delta_row;
                for ( int delta_col = -1; delta_col <= 1; delta_col++ ) {
                    int adjacent_col = gear_loc[1] + delta_col;
                    int number_col_start = num_loc[1];
                    int number_col_end = num_loc[1] + to!int(number_locs[num_loc].length);
                    for ( int number_col = number_col_start; number_col < number_col_end; number_col++ ) {
                        if ( !skip && adjacent_row == num_loc[0] && adjacent_col == number_col ) {
                            skip = true;
                            adjacents ~= to!int(number_locs[num_loc]);
                        }
                    }
                }
            }
        }
        if ( adjacents.length == 2 ) {
            uint prod = 1;
            foreach ( uint adjacent ; adjacents ) {
                prod *= adjacent;
            }
            result += prod;
        }
    }
    return result;
}

uint process(string contents) {
    uint result = 0;
    string[Tuple!(int, int)] number_locs = build_numbers(contents);
    char[Tuple!(int, int)] gear_locs = build_gears(contents);
    return check_gears(number_locs, gear_locs);
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
