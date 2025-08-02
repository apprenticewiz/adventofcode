#!/usr/bin/env dart

import 'dart:io';

import '../../utils/geometry/position2d.dart';

void usage(String progName) {
    print('usage: $progName <input file>');
    exit(1);
}

int process(String filename) {
    Position2D<int> santa = Position2D<int>(0, 0);
    Position2D<int> roboSanta = Position2D<int>(0, 0);
    var positions = <Position2D>{};
    bool santaMove = true;
    positions.add(santa);

    final lines = File(filename).readAsLinesSync();
    for ( final line in lines ) {
        for ( final ch in line.runes ) {
            if ( ch == '^'.codeUnitAt(0) ) {
                santaMove ? (santa = Position2D<int>(santa.x, santa.y + 1)) :
                    (roboSanta = Position2D<int>(roboSanta.x, roboSanta.y + 1));
            } else if ( ch == 'v'.codeUnitAt(0) ) {
                santaMove ? (santa = Position2D<int>(santa.x, santa.y - 1)) :
                    (roboSanta = Position2D<int>(roboSanta.x, roboSanta.y - 1));
            } else if ( ch == '<'.codeUnitAt(0) ) {
                santaMove ? (santa = Position2D<int>(santa.x - 1, santa.y)) :
                    (roboSanta = Position2D<int>(roboSanta.x - 1, roboSanta.y));
            } else if ( ch == '>'.codeUnitAt(0) ) {
                santaMove ? (santa = Position2D<int>(santa.x + 1, santa.y)) :
                    (roboSanta = Position2D<int>(roboSanta.x + 1, roboSanta.y));
            }
            santaMove ? positions.add(santa) : positions.add(roboSanta);
            santaMove = !santaMove;
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
