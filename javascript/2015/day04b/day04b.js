#!/usr/bin/env node

const crypto = require('crypto')
const fs = require('fs');
const path = require('path');
const process = require('process');

function usage() {
    const progname = path.basename(process.argv[1]);
    console.log(`usage: ${progname} <key>`);
    process.exit(1);
}

function processKey(key) {
    let n = 1;
    while ( true ) {
        const tryKey = key + n;
        const digest = crypto.createHash('md5').update(tryKey).digest('hex');
        if ( digest.startsWith('000000') ) {
            return n;
        }
        n++;
    }
}

function main() {
    if ( process.argv.length < 3 ) {
        usage();
    }
    const key = process.argv[2];
    const result = processKey(key);
    console.log(`result = ${result}`);
}

main()
