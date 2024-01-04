#!/usr/bin/env python3

import os
import sys

SAMPLE_AREA = ((7, 7), (27, 27))
INPUT_AREA = ((200000000000000, 200000000000000), (400000000000000, 400000000000000))

class Hailstone:
    def __init__(self, init_pos, velocity):
        self.init_pos = init_pos
        self.velocity = velocity

def usage():
    print(f'usage: {sys.argv[0]} <file>')
    sys.exit(1)

def check_hailstones(hailstones, test_area):
    count = 0
    for i in range(len(hailstones)):
        first = hailstones[i]
        a1x = float(first.init_pos[0])
        a1y = float(first.init_pos[1])
        b1x = float(first.velocity[0])
        b1y = float(first.velocity[1])
        for j in range(i + 1, len(hailstones)):
            second = hailstones[j]
            a2x = float(second.init_pos[0])
            a2y = float(second.init_pos[1])
            b2x = float(second.velocity[0])
            b2y = float(second.velocity[1])
            det_a = (-b1x * b2y) + (b2x * b1y)
            if abs(det_a) > 1.0e-6:
                det_at = (a1x * b2y) + (-a2x * b2y) + (-a1y * b2x) + (a2y * b2x)
                det_as = (-a1y * b1x) + (a2y * b1x) + (a1x * b1y) + (-a2x * b1y)
                t = det_at / det_a
                s = det_as / det_a
                if t > 0.0 and s > 0.0:
                    x = a1x + b1x * t
                    y = a1y + b1y * t
                    if x >= float(test_area[0][0]) and x <= float(test_area[1][0]) \
                        and y >= float(test_area[0][1]) and y <= float(test_area[1][1]):
                            count += 1
    return count

def process(contents, test_area):
    hailstones = []
    for line in contents.splitlines():
        pos_part, velo_part = line.split(' @ ')
        pos_vec = list(map(int, pos_part.split(',')))
        velo_vec = list(map(int, velo_part.split(',')))
        hailstones.append(Hailstone((pos_vec[0], pos_vec[1], pos_vec[2]),
                                    (velo_vec[0], velo_vec[1], velo_vec[2])))
    return check_hailstones(hailstones, test_area)

def main():
    if len(sys.argv) < 2:
        usage()
    filename = sys.argv[1]
    basename = os.path.basename(filename)
    if basename == 'sample.txt':
        test_area = SAMPLE_AREA
    elif basename == 'input.txt':
        test_area = INPUT_AREA
    else:
        raise RuntimeError('input file must be "sample.txt" or "input.txt"')
    try:
        infile = open(filename)
        contents = infile.read()
        result = process(contents, test_area)
        print(f'result = {result}')
    except IOError:
        print(f'read of input file "{filename}" failed.')
        sys.exit(1)

if __name__ == '__main__':
    main()
