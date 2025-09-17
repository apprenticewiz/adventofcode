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
        let memLen = 0;
        let i = 1;
        while ( i < line.length - 1 ) {
            switch ( line[i] ) {
                case '\\':
                    switch ( line[i + 1] ) {
                        case '\\':
                        case '"':
                            i += 2;
                            break;
                        case 'x':
                            i += 4;
                            break;
                        default:
                            i++;
                    }
                    break;
                default:
                    i++;
            }
            memLen++;
        }
        result += codeLen - memLen;
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
