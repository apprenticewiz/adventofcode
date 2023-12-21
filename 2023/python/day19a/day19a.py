#!/usr/bin/env python3

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

class Part:
    def __init__(self, x, m, a, s):
        self.x = x
        self.m = m
        self.a = a
        self.s = s

    def add_ratings(self):
        return self.x + self.m + self.a + self.s

def usage():
    print(f'usage: {sys.argv[0]} <file>')
    sys.exit(1)

def parse_input(contents):
    flows = {}
    parts = []
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
    lineidx += 1
    part_re = re.compile(r'\{x=(\d+),m=(\d+),a=(\d+),s=(\d+)\}')
    while lineidx < len(lines):
        part_matches = part_re.match(lines[lineidx])
        x = int(part_matches.group(1))
        m = int(part_matches.group(2))
        a = int(part_matches.group(3))
        s = int(part_matches.group(4))
        parts.append(Part(x, m, a, s))
        lineidx += 1
    return (flows, parts)

def accept(part, flows):
    curr_label = "in"
    while True:
        workflow = flows[curr_label]
        for rule in workflow.rules:
            cond_true = False
            if rule.predicate.kind == Predicate.LESS_THAN:
                if rule.predicate.rating == "x":
                    cond_true = part.x < rule.predicate.amount
                elif rule.predicate.rating == "m":
                    cond_true = part.m < rule.predicate.amount
                elif rule.predicate.rating == "a":
                    cond_true = part.a < rule.predicate.amount
                elif rule.predicate.rating == "s":
                    cond_true = part.s < rule.predicate.amount
            elif rule.predicate.kind == Predicate.GREATER_THAN:
                if rule.predicate.rating == "x":
                    cond_true = part.x > rule.predicate.amount
                elif rule.predicate.rating == "m":
                    cond_true = part.m > rule.predicate.amount
                elif rule.predicate.rating == "a":
                    cond_true = part.a > rule.predicate.amount
                elif rule.predicate.rating == "s":
                    cond_true = part.s > rule.predicate.amount
            elif rule.predicate.kind == Predicate.UNCONDITIONAL:
                cond_true = True
            if cond_true:
                if rule.action.kind == Action.ACCEPT:
                    return True
                elif rule.action.kind == Action.REJECT:
                    return False
                elif rule.action.kind == Action.GOTO:
                    curr_label = rule.action.label
                    break

def process(contents):
    flows, parts = parse_input(contents)
    rating_sum = 0
    for part in parts:
       if accept(part, flows):
            rating_sum += part.add_ratings()
    return rating_sum

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
