#!/usr/bin/env dart

import 'dart:io';
import 'dart:math';

void usage(String progName) {
    print('usage: $progName <input file>');
    exit(1);
}

int process(String filename) {
    int totalArea = 0;

    final lines = File(filename).readAsLinesSync();
    for ( final line in lines ) {
        List<String> strDims = line.split('x');
        int l = int.parse(strDims[0]);
        int w = int.parse(strDims[1]);
        int h = int.parse(strDims[2]);
        int area1 = l * w;
        int area2 = l * h;
        int area3 = w * h;
        int surfaceArea = 2 * area1 + 2 * area2 + 2 * area3;
        int minArea = min(area1, min(area2, area3));
        totalArea += surfaceArea + minArea;
    }
    return totalArea;
}

void main(List<String> args) {
    if ( args.isEmpty ) {
        usage(Platform.script.pathSegments.last);
    }
    final filename = args[0];
    final result = process(filename);
    print('result = $result');
}
