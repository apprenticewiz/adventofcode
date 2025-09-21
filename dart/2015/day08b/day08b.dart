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
        int encLen = 0;
        for ( int i = 0; i < line.length; i++ ) {
            final ch = line.substring(i, i + 1);
            switch ( ch ) {
            case '\\':
            case '\"':
                encLen += 2;
                break;
            default:
                encLen += 1;
            }
        }
        result += 2 + (encLen - codeLen);
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
