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
    num_rows = len(contents.splitlines())
    num_cols = len(contents.splitlines()[0])
    expanded_rocks = set()
    for rock in rocks:
        for row_mul in range(5):
            for col_mul in range(5):
                expanded_rocks.add((rock[0] + (num_rows * row_mul), rock[1] + (num_cols * col_mul)))
    return Garden(expanded_rocks, (int(num_rows * 5 / 2), int(num_cols * 5 / 2)), (num_rows * 5, num_cols * 5))

def in_bounds(pos, extents):
    return pos[0] >= 0 and pos[0] < extents[0] and pos[1] >= 0 and pos[1] < extents[1]

def walk(garden, steps):
    visited = set()
    visited.add(garden.start)
    directions = [(-1, 0), (1, 0), (0, -1), (0, 1)]
    for i in range(steps):
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
    b0 = walk(garden, 65)
    b1 = walk(garden, 65 + 131)
    b2 = walk(garden, 65 + 2 * 131)
    n = 202300
    det_a = -2.0
    det_a0 = -float(b0) + 2.0 * float(b1) - float(b2)
    det_a1 = 3.0 * float(b0) - 4.0 * float(b1) + float(b2)
    det_a2 = -2.0 * float(b0)
    x0 = int(det_a0 / det_a)
    x1 = int(det_a1 / det_a)
    x2 = int(det_a2 / det_a)
    return x0 * n * n + x1 * n + x2

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
