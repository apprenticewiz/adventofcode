#!/usr/bin/env python3

import sys

def usage():
    progname = sys.argv[0]
    print(f'usage: {progname} <input file>')
    sys.exit(1)

def process(filename):
    result = 0
    with open(filename, 'r') as infile:
        for line in infile:
            code_len = len(line)
            mem_len = 0
            i = 1
            while i < len(line) - 1:
                if line[i] == '\\':
                    if line[i + 1] == '\\' or line[i + 1] == '"':
                        i += 2
                    elif line[i + 1] == 'x':
                        i += 4
                    else:
                        i += 1
                else:
                    i += 1
                mem_len += 1
            result += code_len - mem_len
    return result

def main():
    if len(sys.argv) < 2:
        usage()
    filename = sys.argv[1]
    result = process(filename)
    print(f'result = {result}')

if __name__ == '__main__':
    main()
