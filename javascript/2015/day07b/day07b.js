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
    const operations = new Map();
    var cache = new Map();
    const data = fs.readFileSync(filename, 'utf8');
    const lines = data.trim().split('\n');
    for ( const line of lines ) {
        var match = line.match(/^(\d+|\w+) -> (\w+)$/);
        if ( match != null ) {
            const [_, src, dest] = match;
            operations.set(dest, { operator: 'ASSIGN', src: src });
        }
        match = line.match(/NOT (\d+|\w+) -> (\w+)/);
        if ( match != null ) {
            const [_, src, dest] = match;
            operations.set(dest, { operator: 'NOT', src: src });
        }
        match = line.match(/(\d+|\w+) (AND|OR) (\d+|\w+) -> (\w+)/);
        if ( match != null ) {
            const [_, src1, op, src2, dest] = match;
            operations.set(dest, { operator: op, src1: src1, src2: src2 });
        }
        match = line.match(/(\d+|\w+) (LSHIFT|RSHIFT) (\d+) -> (\w+)/);
        if ( match != null ) {
            const [_, src, op, amtStr, dest] = match;
            operations.set(dest, { operator: op, src: src, amt: parseInt(amtStr) });
        }
    }
    const a = eval(operations, cache, 'a');
    operations.set('b', { operator: 'ASSIGN', src: a.toString() });
    cache = new Map();
    return eval(operations, cache, 'a');
}

function eval(ops, cache, expr) {
    if ( Number.isInteger(Number(expr)) ) {
        return Number(expr);
    }
    else if ( cache.has(expr) ) {
        return cache.get(expr);
    } else {
        let r = 0;
        let a = 0;
        let b = 0;
        const op = ops.get(expr);
        switch ( op.operator ) {
        case 'ASSIGN':
            a = eval(ops, cache, op.src);
            r = a;
            break;
        case 'NOT':
            a = eval(ops, cache, op.src);
            r = ~a;
            break;
        case 'AND':
            a = eval(ops, cache, op.src1);
            b = eval(ops, cache, op.src2);
            r = a & b;
            break;
        case 'OR':
            a = eval(ops, cache, op.src1);
            b = eval(ops, cache, op.src2);
            r = a | b;
            break;
        case 'LSHIFT':
            a = eval(ops, cache, op.src);
            r = a << op.amt;
            break;
        case 'RSHIFT':
            a = eval(ops, cache, op.src);
            r = a >> op.amt;
            break;
        }
        const masked = r & 0xffff;
        cache.set(expr, masked);
        return masked;
    }
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
