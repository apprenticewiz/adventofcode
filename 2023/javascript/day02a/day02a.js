const fs = require('fs');

function usage() {
    console.log(`usage: ${process.argv[0]} ${process.argv[1]} <file>`);
    process.exit(1);
}

function processInput(contents) {
    const totalRed = 12;
    const totalGreen = 13;
    const totalBlue = 14;
    let lines = contents.trim().split('\n');
    let result = 0;
    for ( let line of lines ) {
        let gamePart = line.split(': ')[0];
        let gameNum = parseInt(gamePart.split(' ')[1]);
        let valid = true;
        let drawsPart = line.split(': ')[1];
        let draws = drawsPart.split('; ');
        for ( let drawIndex in draws ) {
            draw = draws[drawIndex];
            colorAmounts = draw.split(', ');
            for ( let colorAmountIndex in colorAmounts ) {
                let colorAmount = colorAmounts[colorAmountIndex];
                let amount = parseInt(colorAmount.split(' ')[0]);
                let color = colorAmount.split(' ')[1];
                if ( color == 'red' && amount > totalRed ) {
                    valid = false;
                } else if ( color == 'green' && amount > totalGreen ) {
                    valid = false;
                } else if ( color == 'blue' && amount > totalBlue ) {
                    valid = false;
                }
            }
        }
        if ( valid ) {
            result += gameNum;
        }
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
