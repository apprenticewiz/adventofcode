#!/usr/bin/env python3

import sys

STEPS = 64

class Garden:
    def __init__(self, rocks, start, extents):
        self.rocks = rocks
        self.start = start
        self.extents = extents

    def __str__(self):
        return f'Garden(rocks={self.rocks}, start={self.start}, extents={self.extents})'

def usage():
    print(f'usage: {sys.argv[0]} <file>')
    sys.exit(1)

def build_garden(contents):
    rocks = set()
    start = (0, 0)
    for row, line in enumerate(contents.splitlines()):
        for col, ch in enumerate(line):
            if ch == '#':
                rocks.add((row, col))
            elif ch == 'S':
                start = (row, col)
    trans_rocks = set()
    for rock in rocks:
        trans_rock = (rock[0] - start[0], rock[1] - start[1])
        trans_rocks.add(trans_rock)
    num_rows = len(contents.splitlines())
    num_cols = len(contents.splitlines()[0])
    extents = ((int(-num_rows / 2), int(-num_cols / 2)), (int(num_rows / 2), int(num_cols / 2)))
    return Garden(trans_rocks, (0, 0), extents)

def in_bounds(pos, extents):
    return pos[0] >= extents[0][0] and pos[0] <= extents[1][0] and pos[1] >= extents[0][1] and pos[1] <= extents[1][1]

def walk(garden):
    visited = set()
    visited.add(garden.start)
    directions = [(-1, 0), (1, 0), (0, -1), (0, 1)]
    for i in range(STEPS):
        new_visited = set()
        for pos in visited:
            for direction in directions:
                new_pos = (pos[0] + direction[0], pos[1] + direction[1])
                if in_bounds(new_pos, garden.extents) and not new_pos in garden.rocks:
                    new_visited.add(new_pos)
        visited = new_visited
    return len(visited)

def process(contents):
    garden = build_garden(contents)
    return walk(garden)

def main():
    if len(sys.argv) < 2:
        usage()
    filename = sys.argv[1]
    try:
        infile = open(filename)
        contents = infile.read()
        result = process(contents)
        print(f'result = {result}')
    except IOError:
        print(f'read of input file "{filename}" failed.')
        sys.exit(1)

if __name__ == '__main__':
    main()
