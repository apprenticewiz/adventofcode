#!/usr/bin/env python3

from collections import deque
from enum import Enum, auto
import copy
import functools
import math
import sys

class Pulse(Enum):
    HIGH = auto()
    LOW = auto()

class ComponentType(Enum):
    BROADCAST = auto()
    CONJUNCTION = auto()
    FLIP_FLOP = auto()
    SINK = auto()

class Component:
    def __init__(self, kind, targets, memory=None, enabled=None):
        self.kind = kind
        self.targets = targets
        self.memory = memory
        self.enabled = enabled
    
    def __str__(self):
        return f'Component(kind={self.kind}, targets={self.targets}, memory={self.memory}, enabled={self.enabled})'

class Message:
    def __init__(self, source, pulse):
        self.source = source
        self.pulse = pulse

    def __str__(self):
        return f'Message(source={self.source}, pulse={self.pulse})'

def usage():
    print(f'usage: {sys.argv[0]} <file>')
    sys.exit(1)

def build_components(contents):
    components = {}
    for line in contents.splitlines():
        source_part, dest_part = line.split(' -> ')
        if source_part[0] == '%':
            label = source_part[1:]
            component = Component(kind=ComponentType.FLIP_FLOP,
                                  targets=dest_part.split(', '),
                                  enabled=False)
            components[label] = component
        elif source_part[0] == '&':
            label = source_part[1:]
            component = Component(kind=ComponentType.CONJUNCTION,
                                  targets=dest_part.split(', '),
                                  memory={})
            components[label] = component
        else:
            if source_part != 'broadcaster':
                print('unknown source module type')
                sys.exit(1)
            else:
                label = copy.deepcopy(source_part)
                component = Component(kind=ComponentType.BROADCAST,
                                      targets=dest_part.split(', '))
                components[label] = component
    conjunctions = [k for k, v in components.items() if v.kind == ComponentType.CONJUNCTION]
    for conjunction in conjunctions:
        sources = [source for source in components if conjunction in components[source].targets]
        for source in sources:
            components[conjunction].memory[source] = Pulse.LOW
    sink_components = {}
    for component in components.values():
        if any(map(lambda x: x not in components.keys(), component.targets)):
            sinks = filter(lambda x: x not in components.keys(), component.targets)
            for sink in sinks:
                sink_components[sink] = Component(ComponentType.SINK, [])
    components.update(sink_components)
    return components

def push_button(components):
    queues = {}
    for label in components.keys():
        queues[label] = deque()
    rx_parent = [x[0] for x in components.items() if 'rx' in x[1].targets][0]
    rx_num_inputs = len([x[0] for x in components.items() if rx_parent in x[1].targets])
    loop_count = 0
    rx_high_counts = {}
    while True:
        loop_count += 1
        current = 'broadcaster'
        queues[current].append(Message('button', Pulse.LOW))
        while any(len(queues[x]) != 0 for x in queues.keys()):
            for label in [x for x in queues.keys() if len(queues[x]) != 0]:
                component = components[label]
                if component.kind == ComponentType.BROADCAST:
                    while queues[label]:
                        message = queues[label].popleft()
                        for target in component.targets:
                            queues[target].append(Message(label, message.pulse))
                elif component.kind == ComponentType.FLIP_FLOP:
                    while queues[label]:
                        message = queues[label].popleft()
                        if message.pulse == Pulse.LOW:
                            for target in component.targets:
                                if component.enabled:
                                    queues[target].append(Message(label, Pulse.LOW))
                                else:
                                    queues[target].append(Message(label, Pulse.HIGH))
                            component.enabled = not component.enabled
                elif component.kind == ComponentType.CONJUNCTION:
                    while queues[label]:
                        message = queues[label].popleft()
                        component.memory[message.source] = message.pulse
                        for target in component.targets:
                            if all(component.memory[x] == Pulse.HIGH for x in component.memory.keys()):
                                queues[target].append(Message(label, Pulse.LOW))
                            else:
                                queues[target].append(Message(label, Pulse.HIGH))
                        if label == rx_parent and message.pulse == Pulse.HIGH \
                            and message.source not in rx_high_counts.keys():
                                rx_high_counts[message.source] = loop_count
                elif component.kind == ComponentType.SINK:
                    while queues[label]:
                        message = queues[label].popleft()
        if len(rx_high_counts.keys()) == rx_num_inputs:
            break
    return functools.reduce(math.lcm, rx_high_counts.values(), 1)

def process(contents):
    components = build_components(contents)
    return push_button(components)

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
