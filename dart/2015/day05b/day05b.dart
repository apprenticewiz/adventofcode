#!/usr/bin/env dart

import 'dart:io';

void usage(String progName) {
    print('usage: $progName <input file>');
    exit(1);
}

bool prop1(String str) {
    for ( int i = 0; i < str.length - 3; i++ ) {
        String first = str.substring(i, i + 2);
        for ( int j = i + 2; j < str.length - 1; j++ ) {
            String second = str.substring(j, j + 2);
            if ( first == second ) {
                return true;
            }
        }
    }
    return false;
}

bool prop2(String str) {
    for ( int i = 2; i < str.length; i++ ) {
        if ( str[i] == str[i - 2] ) {
            return true;
        }
    }
    return false;
}

int process(String filename) {
    int count = 0;

    final lines = File(filename).readAsLinesSync();
    for ( final line in lines ) {
        if ( prop1(line) && prop2(line) ) {
            count++;
        }
    }
    return count;
}

void main(List<String> args) {
    if ( args.isEmpty ) {
        usage(Platform.script.pathSegments.last);
    }
    final filename = args[0];
    final result = process(filename);
    print('result = $result');
}
