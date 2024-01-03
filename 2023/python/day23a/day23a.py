#!/usr/bin/env python3

from collections import deque
import copy
import sys

def usage():
    print(f'usage: {sys.argv[0]} <file>')
    sys.exit(1)

def build_grid(contents):
    grid = []
    for line in contents.splitlines():
        grid.append(list(line))
    return grid

def in_bounds(pos, extents):
    return pos[0] >= 0 and pos[0] < extents[0] and pos[1] >= 0 and pos[1] < extents[1]

def walk_grid(grid, start, end):
    paths = []
    num_rows = len(grid)
    num_cols = len(grid[0])
    extents = (num_rows, num_cols)
    q = deque()
    q.append([start])
    while q:
        path = copy.deepcopy(q.popleft())
        path_end = path[-1]
        if path_end == end:
            paths.append(path)
        else:
            object = grid[path_end[0]][path_end[1]]
            if object == '^' or object == '.':
                neighbor = (path_end[0] - 1, path_end[1])
                if neighbor not in path and in_bounds(neighbor, extents) and \
                    grid[neighbor[0]][neighbor[1]] != '#':
                        new_path = copy.deepcopy(path)
                        new_path.append(neighbor)
                        q.append(new_path)
            if object == 'v' or object == '.':
                neighbor = (path_end[0] + 1, path_end[1])
                if neighbor not in path and in_bounds(neighbor, extents) and \
                    grid[neighbor[0]][neighbor[1]] != '#':
                        new_path = copy.deepcopy(path)
                        new_path.append(neighbor)
                        q.append(new_path)
            if object == '<' or object == '.':
                neighbor = (path_end[0], path_end[1] - 1)
                if neighbor not in path and in_bounds(neighbor, extents) and \
                    grid[neighbor[0]][neighbor[1]] != '#':
                        new_path = copy.deepcopy(path)
                        new_path.append(neighbor)
                        q.append(new_path)
            if object == '>' or object == '.':
                neighbor = (path_end[0], path_end[1] + 1)
                if neighbor not in path and in_bounds(neighbor, extents) and \
                    grid[neighbor[0]][neighbor[1]] != '#':
                        new_path = copy.deepcopy(path)
                        new_path.append(neighbor)
                        q.append(new_path)
    return max([len(x) for x in paths]) - 1

def process(contents):
    grid = build_grid(contents)
    num_rows = len(grid)
    num_cols = len(grid[0])
    return walk_grid(grid, (0, 1), (num_rows - 1, num_cols - 2))

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
