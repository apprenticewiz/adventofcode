#!/usr/bin/env python3

from collections import defaultdict, deque
import copy
import sys

class GraphBuilderState:
    def __init__(self, pos, visited, prev, last, steps):
        self.pos = pos
        self.visited = visited
        self.prev = prev
        self.last = last
        self.steps = steps
    
    def __str__(self):
        return f'GraphBuilderState=(pos={self.pos}, visited={self.visited}, prev={self.prev}, last={self.last}, steps={self.steps})'

class GraphTraversalState:
    def __init__(self, node, path, steps):
        self.node = node
        self.path = path
        self.steps = steps
    
    def __str__(self):
        return f'GraphTraversalState(node={self.node}, path={self.path}, steps={self.steps})'

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

def build_graph(grid):
    start = (0, 1)
    num_rows = len(grid)
    num_cols = len(grid[0])
    SOURCE = 0
    DEST = 1
    extents = (num_rows, num_cols)
    end = (num_rows - 1, num_cols - 2)
    directions = [(-1, 0), (1, 0), (0, -1), (0, 1)]
    branches = { start: SOURCE, end: DEST, }
    graph = defaultdict(set)
    init_state = GraphBuilderState(start, set(), start, 0, 0)
    stack = deque()
    stack.append(init_state)
    while stack:
        curr_state = stack.pop()
        pos = curr_state.pos
        visited = copy.deepcopy(curr_state.visited)
        visited.add(pos)
        count = 0
        prev = curr_state.prev
        for direction in directions:
            next_pos = (pos[0] + direction[0], pos[1] + direction[1])
            if in_bounds(next_pos, extents) and grid[next_pos[0]][next_pos[1]] != '#' and next_pos != prev:
                count += 1
        last = curr_state.last
        steps = curr_state.steps
        if count > 1:
            curr = len(branches.keys())
            branches[pos] = curr
            graph[curr].add((last, steps))
            graph[last].add((curr, steps))
            last = curr
            steps = 0
        for direction in directions:
            next_pos = (pos[0] + direction[0], pos[1] + direction[1])
            if in_bounds(next_pos, extents) and next_pos != prev and next_pos in branches.keys():
                curr = branches[next_pos]
                graph[curr].add((last, steps + 1))
                graph[last].add((curr, steps + 1))
            elif in_bounds(next_pos, extents) and grid[next_pos[0]][next_pos[1]] != '#' and next_pos not in visited:
                stack.append(GraphBuilderState(next_pos, visited, pos, last, steps + 1))
    return graph

def find_longest(graph):
    SOURCE = 0
    DEST = 1
    costs = []
    stack = deque()
    stack.append(GraphTraversalState(SOURCE, [], 0))
    while stack:
        state = stack.pop()
        if state.node == DEST:
            costs.append(state.steps)
        else:
            path = copy.deepcopy(state.path)
            path.append(state.node)
            for dest, cost in graph[state.node]:
                if not dest in path:
                    stack.append(GraphTraversalState(dest, path, state.steps + cost))
    return max(costs)

def process(contents):
    grid = build_grid(contents)
    graph = build_graph(grid)
    return find_longest(graph)

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
