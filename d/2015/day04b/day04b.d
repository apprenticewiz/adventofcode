import std.algorithm;
import std.conv;
import std.digest.md;
import std.exception;
import std.file;
import std.stdio;

void usage(string progname) {
    stderr.writeln("usage: ", progname, " <key>");
}

int process(string key) {
    int n = 1;
    auto md5 = new MD5Digest();
    
    while ( true ) {
        string tryKey = text(key, n);
        ubyte[] hash = md5.digest(tryKey);
        string digest = toHexString(hash);
        if ( digest.startsWith("000000") ) {
            return n;
        }
        n += 1;
    }
}

int main(string[] args) {
    string progname = args[0];

    if ( args.length < 2 ) {
        usage(progname);
        return 1;
    }

    string key = args[1];

    int result = process(key);
    writeln("result = ", result);

    return 0;
}
