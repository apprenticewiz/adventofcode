#!/usr/bin/env python3

from collections import deque
import copy
import re
import sys

class Predicate:
    LESS_THAN = 0
    GREATER_THAN = 1
    UNCONDITIONAL = 2

    def __init__(self, kind, rating=None, amount=None):
        self.kind = kind
        self.rating = rating
        self.amount = amount

class Action:
    ACCEPT = 0
    REJECT = 1
    GOTO = 2

    def __init__(self, kind, label=None):
        self.kind = kind
        self.label = label

class Rule:
    def __init__(self, predicate, action):
        self.predicate = predicate
        self.action = action

class Workflow:
    def __init__(self):
        self.rules = []

class Interval:
    def __init__(self, low, high):
        self.low = low
        self.high = high
    
    def __str__(self):
        return f'({self.low}, {self.high})'

class State:
    def __init__(self, label, x, m, a, s):
        self.label = label
        self.x = x
        self.m = m
        self.a = a
        self.s = s
    
    def __str__(self):
        return f'State(label={self.label}, x={self.x}, m={self.m}, a={self.a}, s={self.s})'

def usage():
    print(f'usage: {sys.argv[0]} <file>')
    sys.exit(1)

def parse_input(contents):
    flows = {}
    lines = contents.splitlines()
    lineidx = 0
    flow_re = re.compile(r'([a-z]+)\{([A-Za-z0-9:<>,]+)\}')
    cond_re = re.compile(r'([xmas])(<|>)(\d+):(A|R|[a-z]+)')
    while True:
        if lines[lineidx] == '':
            break
        flow_match = flow_re.match(lines[lineidx])
        flow_id = flow_match.group(1)
        rules_str = flow_match.group(2)
        workflow = Workflow()
        for rule_str in rules_str.split(','):
            cond_match = cond_re.match(rule_str)
            if cond_match != None:
                rating = cond_match.group(1)
                test = cond_match.group(2)
                value = int(cond_match.group(3))
                action_str = cond_match.group(4)
                if test == '<':
                    predicate = Predicate(kind=Predicate.LESS_THAN, rating=rating, amount=value)
                elif test == '>':
                    predicate = Predicate(kind=Predicate.GREATER_THAN, rating=rating, amount=value)
                if action_str == 'A':
                    action = Action(kind=Action.ACCEPT)
                elif action_str == 'R':
                    action = Action(kind=Action.REJECT)
                else:
                    action = Action(kind=Action.GOTO, label=action_str)
                rule = Rule(predicate, action)
                workflow.rules.append(rule)
            else:
                predicate = Predicate(kind=Predicate.UNCONDITIONAL)
                if rule_str == 'A':
                    action = Action(kind=Action.ACCEPT)
                elif rule_str == 'R':
                    action = Action(kind=Action.REJECT)         
                else:
                    action = Action(kind=Action.GOTO, label=rule_str)
                rule = Rule(predicate, action)
                workflow.rules.append(rule)
        flows[flow_id] = workflow
        lineidx += 1
    return flows

def scan_ranges(flows):
    init_state = State("in", Interval(1, 4000), Interval(1, 4000), Interval(1, 4000), Interval(1, 4000))
    q = deque()
    q.append(init_state)
    accepted = []
    while q:
        state = q.popleft()
        workflow = flows[state.label]
        working_state = copy.deepcopy(state)
        for rule in workflow.rules:
            if rule.predicate.kind == Predicate.UNCONDITIONAL:
                if rule.action.kind == Action.REJECT:
                    # reject this state, do nothing
                    pass
                elif rule.action.kind == Action.ACCEPT:
                    # add this state to the accepted list
                    accepted.append(copy.deepcopy(working_state))
                elif rule.action.kind == Action.GOTO:
                    # test the current working state at the next label
                    working_state.label = rule.action.label
                    q.append(copy.deepcopy(working_state))
            elif rule.predicate.kind == Predicate.LESS_THAN:
                if rule.action.kind == Action.REJECT:
                    # split the range and continue with the non-rejected part
                    if rule.predicate.rating == 'x':
                        working_state.x.low = rule.predicate.amount
                    elif rule.predicate.rating == 'm':
                        working_state.m.low = rule.predicate.amount
                    elif rule.predicate.rating == 'a':
                        working_state.a.low = rule.predicate.amount
                    elif rule.predicate.rating == 's':
                        working_state.s.low = rule.predicate.amount
                elif rule.action.kind == Action.ACCEPT:
                    # split the range, accept the less than part, continue
                    accepted_state = copy.deepcopy(working_state)
                    if rule.predicate.rating == 'x':
                        accepted_state.x.high = rule.predicate.amount - 1
                        working_state.x.low = rule.predicate.amount
                    elif rule.predicate.rating == 'm':
                        accepted_state.m.high = rule.predicate.amount - 1
                        working_state.m.low = rule.predicate.amount
                    elif rule.predicate.rating == 'a':
                        accepted_state.a.high = rule.predicate.amount - 1
                        working_state.a.low = rule.predicate.amount
                    elif rule.predicate.rating == 's':
                        accepted_state.s.high = rule.predicate.amount - 1
                        working_state.s.low = rule.predicate.amount
                    accepted.append(accepted_state)
                elif rule.action.kind == Action.GOTO:
                    # split the range, test on less part at new node, continue
                    goto_state = copy.deepcopy(working_state)
                    if rule.predicate.rating == 'x':
                        goto_state.x.high = rule.predicate.amount - 1
                        working_state.x.low = rule.predicate.amount
                    elif rule.predicate.rating == 'm':
                        goto_state.m.high = rule.predicate.amount - 1
                        working_state.m.low = rule.predicate.amount
                    elif rule.predicate.rating == 'a':
                        goto_state.a.high = rule.predicate.amount - 1
                        working_state.a.low = rule.predicate.amount
                    elif rule.predicate.rating == 's':
                        goto_state.s.high = rule.predicate.amount - 1
                        working_state.s.low = rule.predicate.amount
                    goto_state.label = rule.action.label
                    q.append(goto_state)
            elif rule.predicate.kind == Predicate.GREATER_THAN:
                if rule.action.kind == Action.REJECT:
                    # split the range and continue with the non-rejected part
                    if rule.predicate.rating == 'x':
                        working_state.x.high = rule.predicate.amount
                    elif rule.predicate.rating == 'm':
                        working_state.m.high = rule.predicate.amount
                    elif rule.predicate.rating == 'a':
                        working_state.a.high = rule.predicate.amount
                    elif rule.predicate.rating == 's':
                        working_state.s.high = rule.predicate.amount
                elif rule.action.kind == Action.ACCEPT:
                    # split the range, accept the less than part, continue
                    accepted_state = copy.deepcopy(working_state)
                    if rule.predicate.rating == 'x':
                        accepted_state.x.low = rule.predicate.amount + 1
                        working_state.x.high = rule.predicate.amount
                    elif rule.predicate.rating == 'm':
                        accepted_state.m.low = rule.predicate.amount + 1
                        working_state.m.high = rule.predicate.amount
                    elif rule.predicate.rating == 'a':
                        accepted_state.a.low = rule.predicate.amount + 1
                        working_state.a.high = rule.predicate.amount
                    elif rule.predicate.rating == 's':
                        accepted_state.s.low = rule.predicate.amount + 1
                        working_state.s.high = rule.predicate.amount
                    accepted.append(accepted_state)
                elif rule.action.kind == Action.GOTO:
                    # split the range, test on less part at new node, continue
                    goto_state = copy.deepcopy(working_state)
                    if rule.predicate.rating == 'x':
                        goto_state.x.low = rule.predicate.amount + 1
                        working_state.x.high = rule.predicate.amount
                    elif rule.predicate.rating == 'm':
                        goto_state.m.low = rule.predicate.amount + 1
                        working_state.m.high = rule.predicate.amount
                    elif rule.predicate.rating == 'a':
                        goto_state.a.low = rule.predicate.amount + 1
                        working_state.a.high = rule.predicate.amount
                    elif rule.predicate.rating == 's':
                        goto_state.s.low = rule.predicate.amount + 1
                        working_state.s.high = rule.predicate.amount
                    goto_state.label = rule.action.label
                    q.append(goto_state)
    result = 0
    for state in accepted:
        result += (state.x.high - state.x.low + 1) * (state.m.high - state.m.low + 1) * \
            (state.a.high - state.a.low + 1) * (state.s.high - state.s.low + 1)
    return result

def process(contents):
    flows = parse_input(contents)
    return scan_ranges(flows)

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
