const fs = require('fs');

function usage() {
    console.log(`usage: ${process.argv[0]} ${process.argv[1]} <file>`);
    process.exit(1);
}

function processInput(contents) {
    let lines = contents.trim().split('\n');
    let digits = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
    let result = 0;
    for ( let line of lines ) {
        let minIndex = null;
        let maxIndex = null;
        let leftDigit = null;
        let rightDigit = null;
        for ( let digit of digits ) {
            if ( line.includes(digit) ) {
                let leftIndex = line.indexOf(digit);
                if ( minIndex == null || leftIndex < minIndex ) {
                    minIndex = leftIndex;
                    leftDigit = digit;
                }
                let rightIndex = line.lastIndexOf(digit);
                if ( maxIndex == null || rightIndex > maxIndex ) {
                    maxIndex = rightIndex;
                    rightDigit = digit;
                }
            }
        }
        result += parseInt(leftDigit + rightDigit);
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
