#!/usr/bin/env python3

import sys

def usage():
    progname = sys.argv[0]
    print(f'usage: {progname} <input file>')
    sys.exit(1)

def process(filename):
    positions = set()
    santa = (0, 0)
    roboSanta = (0, 0)
    i = 0
    positions.add(santa)
    with open(filename, 'r') as infile:
        for line in infile:
            for ch in line:
                if i % 2 == 0:
                    if ch == '^':
                        santa = (santa[0], santa[1] + 1)
                    elif ch == 'v':
                        santa = (santa[0], santa[1] - 1)
                    elif ch == '<':
                        santa = (santa[0] - 1, santa[1])
                    elif ch == '>':
                        santa = (santa[0] + 1, santa[1])
                    positions.add(santa)
                else:
                    if ch == '^':
                        roboSanta = (roboSanta[0], roboSanta[1] + 1)
                    elif ch == 'v':
                        roboSanta = (roboSanta[0], roboSanta[1] - 1)
                    elif ch == '<':
                        roboSanta = (roboSanta[0] - 1, roboSanta[1])
                    elif ch == '>':
                        roboSanta = (roboSanta[0] + 1, roboSanta[1])
                    positions.add(roboSanta)
                i += 1
    return len(positions)

def main():
    if len(sys.argv) < 2:
        usage()
    filename = sys.argv[1]
    result = process(filename)
    print(f'result = {result}')

if __name__ == '__main__':
    main()
