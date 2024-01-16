const fs = require('fs');

function usage() {
    console.log(`usage: ${process.argv[0]} ${process.argv[1]} <file>`);
    process.exit(1);
}

function processInput(contents) {
    let lines = contents.trim().split('\n');
    let digitsMap = {
        '0': 0,
        '1': 1,
        '2': 2,
        '3': 3,
        '4': 4,
        '5': 5,
        '6': 6,
        '7': 7,
        '8': 8,
        '9': 9,
        "zero": 0,
        "one": 1,
        "two": 2,
        "three": 3,
        "four": 4,
        "five": 5,
        "six": 6,
        "seven": 7,
        "eight": 8,
        "nine": 9,
    }
    let result = 0;
    for ( let line of lines ) {
        let minIndex = null;
        let maxIndex = null;
        let leftDigit = null;
        let rightDigit = null;
        for ( let digit of Object.keys(digitsMap) ) {
            if ( line.includes(digit) ) {
                let leftIndex = line.indexOf(digit);
                if ( minIndex == null || leftIndex < minIndex ) {
                    minIndex = leftIndex;
                    leftDigit = digitsMap[digit];
                }
                let rightIndex = line.lastIndexOf(digit);
                if ( maxIndex == null || rightIndex > maxIndex ) {
                    maxIndex = rightIndex;
                    rightDigit = digitsMap[digit];
                }
            }
        }
        result += leftDigit * 10 + rightDigit;
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
