import std.exception;
import std.file;
import std.stdio;

void usage(string progname) {
    stderr.writeln("usage: ", progname, " <input file>");
}

bool prop1(char[] str) {
    int len = cast(int) str.length;
    for ( auto i = 0; i < len - 3; i++ ) {
        for ( auto j = i + 2; j < len - 1; j++ ) {
            if ( str[i .. i + 2] == str[j .. j + 2] ) {
                return true;
            }
        }
    }
    return false;
}

bool prop2(char[] str) {
    int len = cast(int) str.length;
    for ( auto i = 2; i < len; i++ ) {
        if ( str[i] == str[i - 2] ) {
            return true;
        }
    }
    return false;
}

int process(string filename) {
    int count = 0;

    foreach ( line; File(filename).byLine() ) {
        if ( prop1(line) && prop2(line) ) {
            count++;
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
