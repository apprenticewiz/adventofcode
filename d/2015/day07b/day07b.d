import std.algorithm;
import std.array;
import std.conv;
import std.exception;
import std.file;
import std.regex;
import std.stdio;
import std.string;

struct Operation {
    enum Operator {
        ASSIGN,
        NOT,
        AND,
        OR,
        LSHIFT,
        RSHIFT,
    }

    Operator op;
    string source1;
    string source2;
    int amount;
}

void usage(string progname) {
    stderr.writeln("usage: ", progname, " <input file>");
}

int eval(ref Operation[string] operations, ref int[string] cache, string expr) {
    if ( isNumeric(expr) ) {
        return to!int(expr);
    } else if ( expr in cache ) {
        return cache[expr];
    } else {
        Operation op = operations[expr];
        int a, b, amt, r;
        final switch ( op.op ) {
            case Operation.Operator.ASSIGN:
                a = eval(operations, cache, op.source1);
                r = a;
                break;
            case Operation.Operator.NOT:
                a = eval(operations, cache, op.source1);
                r = ~a;
                break;
            case Operation.Operator.AND:
                a = eval(operations, cache, op.source1);
                b = eval(operations, cache, op.source2);
                r = a & b;
                break;
            case Operation.Operator.OR:
                a = eval(operations, cache, op.source1);
                b = eval(operations, cache, op.source2);
                r = a | b;
                break;
            case Operation.Operator.LSHIFT:
                a = eval(operations, cache, op.source1);
                amt = op.amount;
                r = a << amt;
                break;
            case Operation.Operator.RSHIFT:
                a = eval(operations, cache, op.source1);
                amt = op.amount;
                r = a >> amt;
                break;
        }
        int masked = r & 0xffff;
        cache[expr] = masked;
        return masked;
    }
}

int process(string filename) {
    Operation[string] operations;
    int[string] cache;
    foreach ( line; File(filename).byLine() ) {
        auto re1 = regex(`^(\d+|\w+) -> (\w+)$`);
        auto re2 = regex(`NOT (\d+|\w+) -> (\w+)`);
        auto re3 = regex(`(\d+|\w+) (AND|OR) (\d+|\w+) -> (\w+)`);
        auto re4 = regex(`(\d+|\w+) (LSHIFT|RSHIFT) (\d+) -> (\w+)`);
        auto caps = matchFirst(line, re1);
        if ( caps.length > 0 ) {
            string src = to!string(caps[1]);
            string dest = to!string(caps[2]);
            Operation op;
            op.op = Operation.Operator.ASSIGN;
            op.source1 = src;
            operations[dest] = op;
        }
        caps = matchFirst(line, re2);
        if ( caps.length > 0 ) {
            string src = to!string(caps[1]);
            string dest = to!string(caps[2]);
            Operation op;
            op.op = Operation.Operator.NOT;
            op.source1 = src;
            operations[dest] = op;
        }
        caps = matchFirst(line, re3);
        if ( caps.length > 0 ) {
            string src1 = to!string(caps[1]);
            string opStr = to!string(caps[2]);
            string src2 = to!string(caps[3]);
            string dest = to!string(caps[4]);
            Operation op;
            op.op = (opStr == "AND") ? Operation.Operator.AND : Operation.Operator.OR;
            op.source1 = src1;
            op.source2 = src2;
            operations[dest] = op;
        }
        caps = matchFirst(line, re4);
        if ( caps.length > 0 ) {
            string src = to!string(caps[1]);
            string opStr = to!string(caps[2]);
            int amt = to!int(caps[3]);
            string dest = to!string(caps[4]);
            Operation op;
            op.op = (opStr == "LSHIFT") ? Operation.Operator.LSHIFT : Operation.Operator.RSHIFT;
            op.source1 = src;
            op.amount = amt;
            operations[dest] = op;
        }
    }
    int a = eval(operations, cache, "a");
    Operation op;
    op.op = Operation.Operator.ASSIGN;
    op.source1 = to!string(a);
    operations["b"] = op;
    cache = null;
    return eval(operations, cache, "a");
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
