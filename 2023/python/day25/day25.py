#!/usr/bin/env python3

from collections import deque, OrderedDict
import copy
import sys

def usage():
    print(f'usage: {sys.argv[0]} <file>')
    sys.exit(1)

def build_graph(contents):
    graph = OrderedDict()
    for line in contents.splitlines():
        source, targets_part = line.split(': ')
        for target in targets_part.split():
            if source not in graph.keys():
                graph[source] = []
            graph[source].append(target)
            if target not in graph.keys():
                graph[target] = []
            graph[target].append(source)
    return graph

def scan_components(graph):
    first_group = 1
    second_group = 0
    components = list(graph.keys())
    first_component = components[0]
    for component in components[1:]:
        connections = 0
        tested = OrderedDict()
        tested[first_component] = None
        for starting_component in graph[first_component]:
            if starting_component == component:
                connections += 1
                continue
            seen = OrderedDict()
            q = deque()
            found = False
            q.append((starting_component, [starting_component]))
            while q and not found and connections < 4:
                other_component, path = q.popleft()
                for c in graph[other_component]:
                    if component == c:
                        connections += 1
                        for p in path:
                            tested[copy.deepcopy(p)] = None
                        found = True
                        break
                    elif not c in seen.keys() and not c in path and not c in tested.keys():
                        new_path = copy.deepcopy(path)
                        new_path.append(c)
                        q.append((c, new_path))
                        seen[c] = None
        if connections >= 4:
            first_group += 1
        else:
            second_group += 1
    return first_group * second_group

def process(contents):
    graph = build_graph(contents)
    return scan_components(graph)

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
