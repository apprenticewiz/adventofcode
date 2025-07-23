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
    let counter = 0;
    const data = fs.readFileSync(filename, 'utf8');
    for ( let i = 0; i < data.length; i++ ) {
        const ch = data[i];
        if ( ch == '(' ) {
            counter++;
        } else if ( ch == ')' ) {
            counter--;
        }
    }
    return counter;
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
