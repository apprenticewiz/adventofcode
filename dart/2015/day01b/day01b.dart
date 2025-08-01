#!/usr/bin/env dart

import 'dart:io';

void usage(String progName) {
    print('usage: $progName <input file>');
    exit(1);
}

int process(String filename) {
    int floors = 0;
    int pos = 0;
    final lines = File(filename).readAsLinesSync();
    for ( final line in lines ) {
        for ( final ch in line.runes ) {
            pos++;
            if ( ch == '('.codeUnitAt(0) ) {
                floors++;
            } else if ( ch == ')'.codeUnitAt(0) ) {
                floors--;
            }
            if ( floors < 0 ) {
                return pos;
            }
        }
    }
    return 0;
}

void main(List<String> args) {
    if ( args.isEmpty ) {
        usage(Platform.script.pathSegments.last);
    }
    final filename = args[0];
    final result = process(filename);
    print('result = $result');
}
