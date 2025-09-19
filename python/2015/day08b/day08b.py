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
            enc_len = 0
            for i in range(len(line)):
                if line[i] == '\\' or line[i] == '"':
                    enc_len += 2
                else:
                    enc_len += 1
            result += 2 + (enc_len - code_len)
    return result

def main():
    if len(sys.argv) < 2:
        usage()
    filename = sys.argv[1]
    result = process(filename)
    print(f'result = {result}')

if __name__ == '__main__':
    main()
