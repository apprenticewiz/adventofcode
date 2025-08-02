#!/usr/bin/env dart

import 'dart:io';
import 'dart:math';

void usage(String progName) {
    print('usage: $progName <input file>');
    exit(1);
}

int process(String filename) {
    int totalLen = 0;

    final lines = File(filename).readAsLinesSync();
    for ( final line in lines ) {
        List<String> strDims = line.split('x');
        int l = int.parse(strDims[0]);
        int w = int.parse(strDims[1]);
        int h = int.parse(strDims[2]);
        int perim1 = 2 * (l + w);
        int perim2 = 2 * (l + h);
        int perim3 = 2 * (w + h);
        int presentLen = min(perim1, min(perim2, perim3));
        int bowLen = l * w * h;
        totalLen += presentLen + bowLen;
    }
    return totalLen;
}

void main(List<String> args) {
    if ( args.isEmpty ) {
        usage(Platform.script.pathSegments.last);
    }
    final filename = args[0];
    final result = process(filename);
    print('result = $result');
}
