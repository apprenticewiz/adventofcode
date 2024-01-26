const fs = require('fs');

function usage() {
    console.log(`usage: ${process.argv[0]} ${process.argv[1]} <file>`);
    process.exit(1);
}

function buildNumbers(contents) {
    var lines = contents.trim().split('\n');
    var numberLocs = new Map();
    var scanningNumber = false;
    var row = 0;
    var number = [];
    var currentPos = { row: -1,  col: -1};
    for ( var line of lines ) {
        for ( var col = 0; col < line.length; col++ ) {
            var ch = line.charAt(col);
            if ( scanningNumber ) {
                if ( ch >= '0' && ch <= '9' ) {
                    number.push(ch);
                } else {
                    numberLocs.set(currentPos, number.join(''));
                    scanningNumber = false;
                    number = [];
                }
            } else {
                if ( ch >= '0' && ch <= '9' ) {
                    number.push(ch);
                    currentPos = { row: row, col: col };
                    scanningNumber = true;
                }
            }
        }
        if ( scanningNumber ) {
            numberLocs.set(currentPos, number.join(''));
            scanningNumber = false;
            number = [];
        }
        row++;
    }
    return numberLocs;
}

function buildParts(contents) {
    var lines = contents.trim().split('\n');
    var partLocs = new Map();
    var row = 0;
    for ( var line of lines ) {
        for ( var col = 0; col < line.length; col++ ) {
            var ch = line[col];
            if ( ch == '*' ) {
                partLocs.set({ row: row, col: col }, ch);
            }
        }
        row++;
    }
    return partLocs;
}

function checkParts(numberLocs, partLocs) {
    var result = 0;
    var neighbors = [ { row: -1, col: -1 }, { row: -1, col: 0 }, { row: -1, col: 1 },
                      { row: 0, col: -1 }, { row: 0, col: 1 },
                      { row: 1, col: -1 }, { row: 1, col: 0 }, { row: 1, col: 1 } ];
    for ( var partLocEntry of partLocs.entries() ) {
        var partLoc = partLocEntry[0];
        var adjacentCount = 0;
        var prod = 1;
        for ( var numberLocEntry of numberLocs.entries() ) {
            var numberLoc = numberLocEntry[0];
            var number = numberLocEntry[1];
            var numRow = numberLoc.row;
            var numColFirst = numberLoc.col;
            var numColLast = numberLoc.col + number.length;
            var found = false;
            for ( var neighbor of neighbors ) {
                var adjacent = { row: partLoc.row + neighbor.row, col: partLoc.col + neighbor.col };
                for ( var numCol = numColFirst; numCol < numColLast; numCol++ ) {
                    if ( numRow == adjacent.row && numCol == adjacent.col ) {
                        adjacentCount++;
                        prod *= parseInt(number);
                        found = true;
                        break;
                    }
                }
                if ( found ) {
                    break;
                }
            }
        }
        if ( adjacentCount == 2 ) {
            result += prod;
        }
    }
    return result;
}

function processInput(contents) {
    numberLocs = buildNumbers(contents);
    partLocs = buildParts(contents);
    return checkParts(numberLocs, partLocs);
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
