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
    let totalArea = 0;
    const data = fs.readFileSync(filename, 'utf8');
    const lines = data.trim().split('\n');
    for ( const line of lines ) {
        const [l, w, h] = line.split('x').map(Number);
        const area1 = l * w;
        const area2 = l * h;
        const area3 = w * h;
        const surfaceArea = (2 * area1) + (2 * area2) + (2 * area3);
        const minArea = Math.min(area1, area2, area3);
        totalArea += surfaceArea + minArea;
    }
    return totalArea;
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
