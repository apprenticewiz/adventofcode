#!/usr/bin/env dart

import 'dart:collection';
import 'dart:io';

sealed class Operation {}

class Assign extends Operation {
  final String src;
  Assign(this.src);
}

class Not extends Operation {
  final String src;
  Not(this.src);
}

class And extends Operation {
  final String src1;
  final String src2;
  And(this.src1, this.src2);
}

class Or extends Operation {
  final String src1;
  final String src2;
  Or(this.src1, this.src2);
}

class LeftShift extends Operation {
  final String src;
  final int amt;
  LeftShift(this.src, this.amt);
}

class RightShift extends Operation {
  final String src;
  final int amt;
  RightShift(this.src, this.amt);
}

void usage(String progName) {
    print('usage: $progName <input file>');
    exit(1);
}

int process(String filename) {
    final HashMap<String, Operation> operations = HashMap();
    HashMap<String, int> cache = HashMap();
    final lines = File(filename).readAsLinesSync();
    for ( final line in lines ) {
        final re1 = RegExp(r'^(\d+|\w+) -> (\w+)$');
        var match = re1.firstMatch(line);
        if ( match != null ) {
            final src = match.group(1)!;
            final dest = match.group(2)!;
            operations[dest] = Assign(src);
        }
        final re2 = RegExp(r'NOT (\d+|\w+) -> (\w+)');
        match = re2.firstMatch(line);
        if ( match != null ) {
            final src = match.group(1)!;
            final dest = match.group(2)!;
            operations[dest] = Not(src);
        }
        final re3 = RegExp(r'(\d+|\w+) (AND|OR) (\d+|\w+) -> (\w+)');
        match = re3.firstMatch(line);
        if ( match != null ) {
            final src1 = match.group(1)!;
            final op = match.group(2)!;
            final src2 = match.group(3)!;
            final dest = match.group(4)!;
            if ( op == 'AND' ) {
                operations[dest] = And(src1, src2);
            } else {
                operations[dest] = Or(src1, src2);
            }
        }
        final re4 = RegExp(r'(\d+|\w+) (LSHIFT|RSHIFT) (\d+) -> (\w+)');
        match = re4.firstMatch(line);
        if ( match != null ) {
            final src = match.group(1)!;
            final op = match.group(2)!;
            final amt = int.parse(match.group(3)!);
            final dest = match.group(4)!;
            if ( op == 'LSHIFT' ) {
                operations[dest] = LeftShift(src, amt);
            } else {
                operations[dest] = RightShift(src, amt);
            }
        }
    }
    final a = eval(operations, cache, 'a');
    operations['b'] = Assign(a.toString());
    cache = HashMap();
    return eval(operations, cache, 'a');
}

int eval(HashMap<String, Operation> ops, HashMap<String, int> cache, String expr) {
    if ( RegExp(r'^\d+$').hasMatch(expr) ) {
        return int.parse(expr);
    } else if ( cache.containsKey(expr) ) {
        return cache[expr]!;
    } else {
        final op = ops[expr]!;
        final r = switch ( op ) {
            Assign(:var src) => eval(ops, cache, src),
            Not(:var src) => ~eval(ops, cache, src),
            And(:var src1, :var src2) => eval(ops, cache, src1) & eval(ops, cache, src2),
            Or(:var src1, :var src2) => eval(ops, cache, src1) | eval(ops, cache, src2),
            LeftShift(:var src, :var amt) => eval(ops, cache, src) << amt,
            RightShift(:var src, :var amt) => eval(ops, cache, src) >> amt,
        };
        final masked = r & 0xffff;
        cache[expr] = masked;
        return masked;
    }
}

void main(List<String> args) {
    if ( args.isEmpty ) {
        usage(Platform.script.pathSegments.last);
    }
    final filename = args[0];
    final result = process(filename);
    print('result = $result');
}
