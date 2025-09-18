import std.algorithm;
import std.exception;
import std.file;
import std.stdio;

void usage(string progname) {
    stderr.writeln("usage: ", progname, " <input file>");
}

int process(string filename) {
    int result = 0;

    foreach ( line; File(filename).byLine() ) {
        int codeLen = cast(int)line.length;
        int memLen = 0;
        auto i = 1;
        while ( i < line.length - 1 ) {
            switch ( line[i] ) {
                case '\\':
                    switch ( line[i + 1] ) {
                        case '\\', '\"':
                            i += 2;
                            break;
                        case 'x':
                            i += 4;
                            break;
                        default:
                            i++;
                    }
                    break;
                default:
                    i++;
            }
            memLen++;
        }
        result += (codeLen - memLen);
    }

    return result;
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
