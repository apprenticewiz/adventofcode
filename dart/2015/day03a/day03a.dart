#!/usr/bin/env dart

import 'dart:io';

import '../../utils/geometry/position2d.dart';

void usage(String progName) {
    print('usage: $progName <input file>');
    exit(1);
}

int process(String filename) {
    Position2D<int> santa = Position2D<int>(0, 0);
    var positions = <Position2D>{};
    positions.add(santa);

    final lines = File(filename).readAsLinesSync();
    for ( final line in lines ) {
        for ( final ch in line.runes ) {
            if ( ch == '^'.codeUnitAt(0) ) {
                santa = Position2D<int>(santa.x, santa.y + 1);
            } else if ( ch == 'v'.codeUnitAt(0) ) {
                santa = Position2D<int>(santa.x, santa.y - 1);
            } else if ( ch == '<'.codeUnitAt(0) ) {
                santa = Position2D<int>(santa.x - 1, santa.y);
            } else if ( ch == '>'.codeUnitAt(0) ) {
                santa = Position2D<int>(santa.x + 1, santa.y);
            }
            positions.add(santa);
        }
    }
    return positions.length;
}

void main(List<String> args) {
    if ( args.isEmpty ) {
        usage(Platform.script.pathSegments.last);
    }
    final filename = args[0];
    final result = process(filename);
    print('result = $result');
}
