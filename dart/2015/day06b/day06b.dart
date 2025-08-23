#!/usr/bin/env dart

import 'dart:io';

const int MAX_ROW = 1000;
const int MAX_COL = 1000;

void usage(String progName) {
    print('usage: $progName <input file>');
    exit(1);
}

void perform(List<int> grid, String action, int r1, int c1, int r2, int c2) {
    for ( var row = r1; row <= r2; row++ ) {
        for ( var col = c1; col <= c2; col++ ) {
            var index = row * MAX_COL + col;
            switch ( action ) {
            case 'turn on':
                grid[index] += 1;
                break;
            case 'turn off':
                grid[index] = (grid[index] > 0) ? grid[index] - 1 : 0;
                break;
            case 'toggle':
                grid[index] += 2;
                break;
            }
        }
    }
}

int sum(List<int> grid) {
    var total = 0;
    for ( var row = 0; row < MAX_ROW; row++ ) {
        for ( var col = 0; col < MAX_COL; col++ ) {
            var index = row * MAX_COL + col;
            total += grid[index];
        }
    }
    return total;
}

int process(String filename) {
    var grid = List<int>.filled(MAX_ROW * MAX_COL, 0);
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
    return sum(grid);
}

void main(List<String> args) {
    if ( args.isEmpty ) {
        usage(Platform.script.pathSegments.last);
    }
    final filename = args[0];
    final result = process(filename);
    print('result = $result');
}
