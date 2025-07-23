#!/usr/bin/env python3

import sys

def usage():
    progname = sys.argv[0]
    print(f'usage: {progname} <input file>')
    sys.exit(1)

def process(filename):
    totalLength = 0
    with open(filename, 'r') as infile:
        for line in infile:
            (l, w, h) = tuple(map(int, line.split('x')))
            perim1 = 2 * (l + w)
            perim2 = 2 * (l + h)
            perim3 = 2 * (w + h)
            presentLength = min(perim1, perim2, perim3)
            bowLength = l * w * h
            totalLength += presentLength + bowLength
    return totalLength

def main():
    if len(sys.argv) < 2:
        usage()
    filename = sys.argv[1]
    result = process(filename)
    print(f'result = {result}')

if __name__ == '__main__':
    main()
