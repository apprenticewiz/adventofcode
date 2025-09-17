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
    let result = 0;
    const data = fs.readFileSync(filename, 'utf8');
    const lines = data.trim().split('\n');
    for ( const line of lines ) {
        const codeLen = line.length;
        var encLen = 0;
        for ( let i = 0; i < line.length; i++ ) {
            switch ( line[i] ) {
                case '\\':
                case '"':
                    encLen += 2;
                    break;
                default:
                    encLen++;
            }
        }
        result += 2 + (encLen - codeLen);
    }
    return result;
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
