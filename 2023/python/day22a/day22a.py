#!/usr/bin/env python3

import sys

def usage():
    print(f'usage: {sys.argv[0]} <file>')
    sys.exit(1)

def parse_input(contents):
    bricks = []
    for line in contents.splitlines():
        start_str, end_str = line.split('~')
        sv = list(map(int, start_str.split(',')))
        ev = list(map(int, end_str.split(',')))
        brick = set()
        for z in range(sv[2], ev[2] + 1):
            for y in range(sv[1], ev[1] + 1):
                for x in range(sv[0], ev[0] + 1):
                    brick.add((x, y, z))
        bricks.append(brick)
    bricks.sort(key=lambda x: min(coord[2] for coord in x))
    return bricks

def drop_bricks(bricks):
    occupied = {}
    supports = {}
    for i in range(len(bricks)):
        supports[i] = set()
    for i in range(len(bricks)):
        brick = bricks[i]
        next_pos = set([(x[0], x[1], x[2] - 1) for x in brick])
        intersected = set()
        for pos in next_pos:
            if pos in occupied.keys():
                intersected.add(occupied[pos])
        while not intersected and not any([p[2] == 0 for p in next_pos]):
            brick = next_pos
            next_pos = set([(x[0], x[1], x[2] - 1) for x in brick])
            intersected = set()
            for pos in next_pos:
                if pos in occupied.keys():
                    intersected.add(occupied[pos])
        occupied_i = {}
        for pos in brick:
            occupied_i[pos] = i
        occupied.update(occupied_i)
        for parent in intersected:
            supports[parent].add(i)
    return supports

def calc_supported(supports):
    supported = {}
    for parent, children in supports.items():
        for child in children:
            if child not in supported.keys():
                supported[child] = set([parent])
            else:
                supported[child].add(parent)
    return supported

def count_supports(supports, supported):
    safe = set()
    for parent, children in supports.items():
        if not children or all(map(lambda x: len(supported[x]) > 1, children)):
            safe.add(parent)
    return len(safe)

def process(contents):
    bricks = parse_input(contents)
    supports = drop_bricks(bricks)
    supported = calc_supported(supports)
    return count_supports(supports, supported)

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
