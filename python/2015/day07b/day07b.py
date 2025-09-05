#!/usr/bin/env python3

import re
import sys

def usage():
    progname = sys.argv[0]
    print(f'usage: {progname} <input file>')
    sys.exit(1)

def process(filename):
    operations = {}
    pattern = re.compile(r'(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)')
    p1 = re.compile(r'^(\d+|\w+) -> (\w+)')
    p2 = re.compile(r'NOT (\d+|\w+) -> (\w+)')
    p3 = re.compile(r'(\d+|\w+) (AND|OR) (\d+|\w+) -> (\w+)')
    p4 = re.compile(r'(\d+|\w+) (LSHIFT|RSHIFT) (\d+) -> (\w+)')
    with open(filename, 'r') as infile:
        for line in infile:
            matches = p1.match(line)
            if matches != None:
                src = matches[1]
                dest = matches[2]
                operations[dest] = ('ASSIGN', src)
            matches = p2.match(line)
            if matches != None:
                src = matches[1]
                dest = matches[2]
                operations[dest] = ('NOT', src)
            matches = p3.match(line)
            if matches != None:
                src1 = matches[1]
                op = matches[2]
                src2 = matches[3]
                dest = matches[4]
                operations[dest] = (op, src1, src2)
            matches = p4.match(line)
            if matches != None:
                src = matches[1]
                op = matches[2]
                amt = int(matches[3])
                dest = matches[4]
                operations[dest] = (op, src, amt)
    a = evaluate(operations, {}, "a")
    operations["b"] = ("ASSIGN", str(a))
    return evaluate(operations, {}, "a")

def evaluate(operations, cache, expr):
    try:
        return int(expr)
    except ValueError:
        pass
    if ( expr in cache ):
        return cache[expr]
    else:
        op = operations[expr]
        operator = op[0]
        if operator == 'ASSIGN':
            src = op[1]
            a = evaluate(operations, cache, src)
            r = a
        elif operator == 'NOT':
            src = op[1]
            a = evaluate(operations, cache, src)
            r = ~a
        elif operator == 'AND':
            src1 = op[1]
            src2 = op[2]
            a = evaluate(operations, cache, src1)
            b = evaluate(operations, cache, src2)
            r = a & b
        elif operator == 'OR':
            src1 = op[1]
            src2 = op[2]
            a = evaluate(operations, cache, src1)
            b = evaluate(operations, cache, src2)
            r = a | b
        elif operator == 'LSHIFT':
            src = op[1]
            amt = op[2]
            a = evaluate(operations, cache, src)
            r = a << amt
        elif operator == 'RSHIFT':
            src = op[1]
            amt = op[2]
            a = evaluate(operations, cache, src)
            r = a >> amt
        else:
            raise RuntimeError
        masked = r & 0xffff
        cache[expr] = masked
        return masked

def main():
    if len(sys.argv) < 2:
        usage()
    filename = sys.argv[1]
    result = process(filename)
    print(f'result = {result}')

if __name__ == '__main__':
    main()
