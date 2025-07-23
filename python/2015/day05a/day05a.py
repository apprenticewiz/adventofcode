#!/usr/bin/env python3

import sys

def usage():
    progname = sys.argv[0]
    print(f'usage: {progname} <input file>')
    sys.exit(1)

def prop1(line):
    vowels = 0
    for ch in line:
        if ch in "aeiou":
            vowels += 1
    return vowels >= 3

def prop2(line):
    for i in range(len(line) - 1):
        if line[i] == line[i + 1]:
            return True
    return False

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
