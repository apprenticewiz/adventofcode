#!/usr/bin/env node

const fs = require('fs');
const path = require('path');
const process = require('process');

const MAX_ROW = 1000;
const MAX_COL = 1000;

function usage() {
    const progname = path.basename(process.argv[1]);
    console.log(`usage: ${progname} <input file>`);
    process.exit(1);
}

function perform(grid, action, r1, c1, r2, c2) {
    for ( let row = r1; row <= r2; row++ ) {
        for ( let col = c1; col <= c2; col++ ) {
            switch ( action ) {
            case "turn on":
                grid[row*MAX_COL + col] += 1;
                break;
            case "turn off":
                grid[row*MAX_COL + col] = (grid[row*MAX_COL + col] > 0) ? grid[row*MAX_COL + col] - 1 : 0;
                break;
            case "toggle":
                grid[row*MAX_COL + col] += 2;
                break;
            }
        }
    }
}

function sum(grid) {
    let sum = 0;
    for ( let row = 0; row < MAX_ROW; row++ ) {
        for ( let col = 0; col < MAX_COL; col++ ) {
           sum += grid[row*MAX_COL + col];
        }
    }
    return sum;
}

function processFile(filename) {
    let grid = Array.from({ length: MAX_ROW*MAX_COL }, () => 0);
    const data = fs.readFileSync(filename, 'utf8');
    const lines = data.trim().split('\n');
    for ( const line of lines ) {
        const match = line.match(/(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)/);
        const [_, action, r1, c1, r2, c2] = match;
        perform(grid, action, parseInt(r1), parseInt(c1), parseInt(r2), parseInt(c2));
    }
    return sum(grid);
}

function main() {
    if ( process.argv.length < 3 ) {
        usage();
    }
    const filename = process.argv[2];
    const result = processFile(filename);
    console.log(`result = ${result}`);
}

main()
