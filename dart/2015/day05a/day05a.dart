#!/usr/bin/env dart

import 'dart:io';

void usage(String progName) {
    print('usage: $progName <input file>');
    exit(1);
}

bool prop1(String str) {
    return RegExp(r'[aeiou]').allMatches(str).length >= 3;
}

bool prop2(String str) {
    for ( int i = 1; i < str.length; i++ ) {
        if ( str[i] == str[i - 1] ) {
            return true;
        }
    }
    return false;
}

bool prop3(String str) {
    return !str.contains("ab") && !str.contains("cd") && !str.contains("pq") && !str.contains("xy");
}

int process(String filename) {
    int count = 0;

    final lines = File(filename).readAsLinesSync();
    for ( final line in lines ) {
        if ( prop1(line) && prop2(line) && prop3(line) ) {
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
