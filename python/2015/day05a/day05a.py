#!/usr/bin/env python3

import re
import sys

def usage():
    progname = sys.argv[0]
    print(f'usage: {progname} <input file>')
    sys.exit(1)

def prop1(line):
    return len(re.findall(r'[aeiou]', line)) >= 3

def prop2(line):
    return bool(re.search(r'(.)\1', line))

def prop3(line):
    return not any(map(lambda x: x in line, ['ab', 'cd', 'pq', 'xy']))

def process(filename):
    counter = 0
    with open(filename, 'r') as infile:
        for line in infile:
            if prop1(line) and prop2(line) and prop3(line):
                counter += 1
    return counter

def main():
    if len(sys.argv) < 2:
        usage()
    filename = sys.argv[1]
    result = process(filename)
    print(f'result = {result}')

if __name__ == '__main__':
    main()
