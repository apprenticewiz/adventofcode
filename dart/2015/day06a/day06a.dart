#!/usr/bin/env dart

import 'dart:io';

const int MAX_ROW = 1000;
const int MAX_COL = 1000;

void usage(String progName) {
    print('usage: $progName <input file>');
    exit(1);
}

void perform(List<bool> grid, String action, int r1, int c1, int r2, int c2) {
    for ( var row = r1; row <= r2; row++ ) {
        for ( var col = c1; col <= c2; col++ ) {
            var index = row * MAX_COL + col;
            switch ( action ) {
            case 'turn on':
                grid[index] = true;
                break;
            case 'turn off':
                grid[index] = false;
                break;
            case 'toggle':
                grid[index] = !grid[index];
                break;
            }
        }
    }
}

int count(List<bool> grid) {
    var total = 0;
    for ( var row = 0; row < MAX_ROW; row++ ) {
        for ( var col = 0; col < MAX_COL; col++ ) {
            var index = row * MAX_COL + col;
            if ( grid[index] ) {
                total++;
            }
        }
    }
    return total;
}

int process(String filename) {
    var grid = List<bool>.filled(MAX_ROW * MAX_COL, false);
    final lines = File(filename).readAsLinesSync();
    for ( final line in lines ) {
        var re = RegExp(r'(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)');
        var match = re.firstMatch(line);
        if ( match != null ) {
            var action = match.group(1)!;
            var r1 = int.parse(match.group(2)!);
            var c1 = int.parse(match.group(3)!);
            var r2 = int.parse(match.group(4)!);
            var c2 = int.parse(match.group(5)!);
            perform(grid, action, r1, c1, r2, c2);
        }
    }
    return count(grid);
}

void main(List<String> args) {
    if ( args.isEmpty ) {
        usage(Platform.script.pathSegments.last);
    }
    final filename = args[0];
    final result = process(filename);
    print('result = $result');
}
