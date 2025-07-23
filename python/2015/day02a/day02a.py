#!/usr/bin/env python3

import sys

def usage():
    progname = sys.argv[0]
    print(f'usage: {progname} <input file>')
    sys.exit(1)

def process(filename):
    totalArea = 0
    with open(filename, 'r') as infile:
        for line in infile:
            (l, w, h) = tuple(map(int, line.split('x')))
            area1 = l * w
            area2 = l * h
            area3 = w * h
            surfaceArea = (2 * area1) + (2 * area2) + (2 * area3)
            minArea = min(area1, area2, area3)
            totalArea += surfaceArea + minArea
    return totalArea

def main():
    if len(sys.argv) < 2:
        usage()
    filename = sys.argv[1]
    result = process(filename)
    print(f'result = {result}')

if __name__ == '__main__':
    main()
