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
    let totalLength = 0;
    const data = fs.readFileSync(filename, 'utf8');
    const lines = data.trim().split('\n');
    for ( const line of lines ) {
        const [l, w, h] = line.split('x').map(Number);
        const perim1 = 2 * (l + w);
        const perim2 = 2 * (l + h);
        const perim3 = 2 * (w + h);
        const presentLength = Math.min(perim1, perim2, perim3);
        const bowLength = l * w * h;
        totalLength += presentLength + bowLength;
    }
    return totalLength;
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
