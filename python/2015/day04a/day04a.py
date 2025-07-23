#!/usr/bin/env python3

import hashlib
import sys

def usage():
    progname = sys.argv[0]
    print(f'usage: {progname} <key>')
    sys.exit(1)

def process(key):
    n = 1
    while True:
        tryKey = key + str(n)
        md5 = hashlib.md5()
        md5.update(tryKey.encode('utf-8'))
        digest = md5.hexdigest()
        if digest.startswith('00000'):
            return n
        n += 1

def main():
    if len(sys.argv) < 2:
        usage()
    key = sys.argv[1]
    result = process(key)
    print(f'result = {result}')

if __name__ == '__main__':
    main()
