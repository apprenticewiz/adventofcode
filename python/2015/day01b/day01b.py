#!/usr/bin/env python3

import sys

def usage():
    progname = sys.argv[0]
    print(f'usage: {progname} <input file>')
    sys.exit(1)

def process(filename):
    counter = 0
    pos = 0
    with open(filename, 'r') as infile:
        for line in infile:
            for ch in line:
                pos += 1
                if ch == '(':
                    counter += 1
                elif ch == ')':
                    counter -= 1
                if counter < 0:
                    return pos
    return 0

def main():
    if len(sys.argv) < 2:
        usage()
    filename = sys.argv[1]
    result = process(filename)
    print(f'result = {result}')

if __name__ == '__main__':
    main()
