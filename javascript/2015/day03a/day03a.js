#!/usr/bin/env node

const fs = require('fs');
const path = require('path');
const process = require('process');

function usage() {
    const progname = path.basename(process.argv[1]);
    console.log(`usage: ${progname} <input file>`);
    process.exit(1);
}

function processFile(filename) {
    let positions = new Set();
    let santa = [0, 0];
    const data = fs.readFileSync(filename, 'utf8');
    for ( const ch of data ) {
        switch ( ch ) {
            case '^':
                santa[1] += 1;
                break;
            case 'v':
                santa[1] -= 1;
                break;
            case '<':
                santa[0] -= 1;
                break;
            case '>':
                santa[0] += 1;
                break;
        }
        positions.add(santa.toString());
    }
    return positions.size;
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
