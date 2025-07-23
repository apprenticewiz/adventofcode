#!/usr/bin/env python3

import sys

def usage():
    progname = sys.argv[0]
    print(f'usage: {progname} <input file>')
    sys.exit(1)

def prop1(line):
    for i in range(len(line) - 3):
        firstPair = line[i:i+2]
        for j in range(i + 2, len(line) - 1):
            secondPair = line[j:j+2]
            if firstPair == secondPair:
                return True
    return False

def prop2(line):
    for i in range(len(line) - 2):
        if line[i] == line[i + 2]:
            return True
    return False

def process(filename):
    counter = 0
    with open(filename, 'r') as infile:
        for line in infile:
            if prop1(line) and prop2(line):
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
