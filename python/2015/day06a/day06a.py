#!/usr/bin/env python3

import re
import sys

def usage():
    progname = sys.argv[0]
    print(f'usage: {progname} <input file>')
    sys.exit(1)

def perform(grid, action, r1, c1, r2, c2):
    for row in range(r1, r2+1):
        for col in range(c1, c2+1):
            if action == 'turn on':
                grid.add((row, col))
            elif action == 'turn off':
                grid.discard((row, col))
            elif action == 'toggle':
                grid.discard((row, col)) if (row, col) in grid else grid.add((row, col))

def process(filename):
    grid = set()
    pattern = re.compile(r'(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)')
    with open(filename, 'r') as infile:
        for line in infile:
            matches = pattern.match(line)
            if matches != None:
                action = matches[1]
                r1 = int(matches[2])
                c1 = int(matches[3])
                r2 = int(matches[4])
                c2 = int(matches[5])
                perform(grid, action, r1, c1, r2, c2)
    return len(grid)

def main():
    if len(sys.argv) < 2:
        usage()
    filename = sys.argv[1]
    result = process(filename)
    print(f'result = {result}')

if __name__ == '__main__':
    main()
