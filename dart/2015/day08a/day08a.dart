#!/usr/bin/env dart

import 'dart:io';

void usage(String progName) {
    print('usage: $progName <input file>');
    exit(1);
}

int process(String filename) {
    int result = 0;

    final lines = File(filename).readAsLinesSync();
    for ( final line in lines ) {
        final codeLen = line.length;
        int memLen = 0;
        int i = 1;
        while ( i < line.length - 1 ) {
            final ch1 = line.substring(i, i + 1);
            switch ( ch1 ) {
            case '\\':
                final ch2 = line.substring(i + 1, i + 2);
                switch( ch2 ) {
                case '\\':
                case '\"':
                    i += 2;
                    break;
                case 'x':
                    i += 4;
                    break;
                default:
                    i += 1;
                }
                break;
            default:
                i += 1;
            }
            memLen += 1;
        }
        result += codeLen - memLen;
    }
    return result;
}

void main(List<String> args) {
    if ( args.isEmpty ) {
        usage(Platform.script.pathSegments.last);
    }
    final filename = args[0];
    final result = process(filename);
    print('result = $result');
}
