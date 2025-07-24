#!/usr/bin/env node

const fs = require('fs');
const path = require('path');
const process = require('process');

function usage() {
    const progname = path.basename(process.argv[1]);
    console.log(`usage: ${progname} <input file>`);
    process.exit(1);
}

function prop1(str) {
    return /(..).*\1/.test(str);
}

function prop2(str) {
    return /(.).\1/.test(str);
}

function processFile(filename) {
    let count = 0;
    const data = fs.readFileSync(filename, 'utf8');
    const lines = data.trim().split('\n');
    for ( const line of lines ) {
        if ( prop1(line) && prop2(line) ) {
            count++;
        }
    }
    return count;
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
