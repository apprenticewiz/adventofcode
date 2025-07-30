import std.algorithm;
import std.exception;
import std.file;
import std.stdio;

void usage(string progname) {
    stderr.writeln("usage: ", progname, " <input file>");
}

bool prop1(char[] str) {
    int vowels = 0;
    for ( auto i = 0; i < str.length; i++ ) {
        if (str[i] == 'a' || str[i] == 'e' || str[i] == 'i' || str[i] == 'o' || str[i] == 'u' ) {
            vowels++;
        }
    }
    return vowels >= 3;
}

bool prop2(char[] str) {
    for ( auto i = 1; i < str.length; i++ ) {
        if ( str[i] == str[i - 1] ) {
            return true;
        }
    }
    return false;
}

bool prop3(char[] str) {
    return count(str, "ab") == 0 && count(str, "cd") == 0 && count(str, "pq") == 0 && count(str, "xy") == 0;
}

int process(string filename) {
    int count = 0;

    foreach ( line; File(filename).byLine() ) {
        if ( prop1(line) && prop2(line) && prop3(line) ) {
            count += 1;
        }
    }

    return count;
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
