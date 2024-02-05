const fs = require('fs');

function usage() {
    console.log(`usage: ${process.argv[0]} ${process.argv[1]} <file>`);
    process.exit(1);
}

function processInput(contents) {
    let lines = contents.trim().split('\n');
    let result = 0;
    let instances = new Map();
    for ( let line of lines ) {
        let cardPart = line.split(': ')[0];
        let cardNumParts = cardPart.split(' ');
        let cardNum = parseInt(cardNumParts[cardNumParts.length - 1]);
        let rest = line.split(': ')[1];
        let winningStr = rest.split(' | ')[0];
        let winningSet = new Set();
        for ( let numStr of winningStr.trim().split(' ') ) {
            if ( numStr != '' ) {
                let num = parseInt(numStr);
                winningSet.add(num);
            }
        }
        let handStr = rest.split(' | ')[1];
        let handSet = new Set();
        for ( let numStr of handStr.trim().split(' ') ) {
            if ( numStr != '' ) {
                let num = parseInt(numStr);
                handSet.add(num);
            }
        }
        let intersection = new Set();
        for ( let num of winningSet ) {
            if ( handSet.has(num) ) {
                intersection.add(num);
            }
        }
        let count = intersection.size;
        for ( let i = cardNum + 1; i <= cardNum + count; i++ ) {
            let count = 0;
            if ( instances.has(i) ) {
                count += instances.get(i);
            }
            count++;
            if ( instances.has(cardNum) ) {
                count += instances.get(cardNum);
            }
            instances.set(i, count);
        }
        result++;
    }
    for ( let value of instances.values() ) {
        result += value;
    }
    return result;
}

function main() {
    if ( process.argv.length < 3 ) {
        usage();
    }
    let filename = process.argv[2];
    let contents = fs.readFileSync(filename, 'utf8');
    let result = processInput(contents);
    console.log(`result = ${result}`);
}

main()
