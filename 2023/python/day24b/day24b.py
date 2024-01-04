#!/usr/bin/env python3

import math
import sys

class Hailstone:
    def __init__(self, init_pos, velocity):
        self.init_pos = init_pos
        self.velocity = velocity

def usage():
    print(f'usage: {sys.argv[0]} <file>')
    sys.exit(1)

# Credit to Tom Huntington for solution
# See: https://gist.github.com/tom-huntington/00065d3f5c52a900bfa99a6230470956

def sub(u, v):
    return [u[0] - v[0], u[1] - v[1], u[2] - v[2]]

def exterior3(u, v, w):
    return u[0] * v[1] * w[2] + u[1] * v[2] * w[0] + u[2] * v[0] * w[1] \
        - u[0] * v[2] * w[1] - u[1] * v[0] * w[2] - u[2] * v[1] * w[0]

def exterior2(v, w):
    return [
        v[0] * w[1] - v[1] * w[0],
        v[1] * w[2] - v[2] * w[1],
        v[2] * w[0] - v[0] * w[2],
    ]

def calc_start(hailstones):
    d = [
        [
            [float(hailstones[0].init_pos[0]),
             float(hailstones[0].init_pos[1]),
             float(hailstones[0].init_pos[2]),],
            [float(hailstones[0].velocity[0]),
             float(hailstones[0].velocity[1]),
             float(hailstones[0].velocity[2]),],
        ],
        [
            [float(hailstones[1].init_pos[0]),
             float(hailstones[1].init_pos[1]),
             float(hailstones[1].init_pos[2]),],
            [float(hailstones[1].velocity[0]),
             float(hailstones[1].velocity[1]),
             float(hailstones[1].velocity[2]),],
        ],
        [
            [float(hailstones[2].init_pos[0]),
             float(hailstones[2].init_pos[1]),
             float(hailstones[2].init_pos[2]),],
            [float(hailstones[2].velocity[0]),
             float(hailstones[2].velocity[1]),
             float(hailstones[2].velocity[2]),],
        ],
    ]
    a = [
        exterior2(sub(d[0][1], d[1][1]), sub(d[0][0], d[1][0])),
        exterior2(sub(d[0][1], d[2][1]), sub(d[0][0], d[2][0])),
        exterior2(sub(d[1][1], d[2][1]), sub(d[1][0], d[2][0])),
    ]
    b = [
        -exterior3(d[0][0], d[0][1], d[1][0]) - exterior3(d[1][0], d[1][1], d[0][0]),
        -exterior3(d[0][0], d[0][1], d[2][0]) - exterior3(d[2][0], d[2][1], d[0][0]),
        -exterior3(d[1][0], d[1][1], d[2][0]) - exterior3(d[2][0], d[2][1], d[1][0]),
    ]
    det_a = a[0][0] * a[1][1] * a[2][2] - a[0][0] * a[1][2] * a[2][1] - a[0][1] * a[1][0] * a[2][2] \
        + a[0][1] * a[1][2] * a[2][0] + a[0][2] * a[1][0] * a[2][1] - a[0][2] * a[1][1] * a[2][0]
    det_ax = b[0] * a[1][1] * a[2][2] - b[0] * a[1][2] * a[2][1] - a[0][1] * b[1] * a[2][2] \
        + a[0][1] * a[1][2] * b[2] + a[0][2] * b[1] * a[2][1] - a[0][2] * a[1][1] * b[2]
    det_ay = a[0][0] * b[1] * a[2][2] - a[0][0] * a[1][2] * b[2] - b[0] * a[1][0] * a[2][2] \
        + b[0] * a[1][2] * a[2][0] + a[0][2] * a[1][0] * b[2] - a[0][2] * b[1] * a[2][0]
    det_az = a[0][0] * a[1][1] * b[2] - a[0][0] * b[1] * a[2][1] - a[0][1] * a[1][0] * b[2] \
        + a[0][1] * b[1] * a[2][0] + b[0] * a[1][0] * a[2][1] - b[0] * a[1][1] * a[2][0]
    x = det_ax / det_a
    y = det_ay / det_a
    z = det_az / det_a
    return int(math.ceil(x) + math.ceil(y) + math.ceil(z))

def process(contents):
    hailstones = []
    for line in contents.splitlines():
        pos_part, velo_part = line.split(' @ ')
        pos_vec = list(map(int, pos_part.split(',')))
        velo_vec = list(map(int, velo_part.split(',')))
        hailstones.append(Hailstone((pos_vec[0], pos_vec[1], pos_vec[2]),
                                    (velo_vec[0], velo_vec[1], velo_vec[2])))
    return calc_start(hailstones)

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
