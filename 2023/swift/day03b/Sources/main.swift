import Foundation

func usage() {
    let progname = (CommandLine.arguments[0] as NSString).lastPathComponent
    print("usage: swift run \(progname) <file>")
    exit(1)
}

func buildNumbers(contents: String) -> [(Int, Int, String)] {
    var numLocs: [(Int, Int, String)] = []
    var row = 0
    var scanningNumber = false
    var number = ""
    var currentPos = (-1, -1)
    for line in contents.split(separator: "\n") {
        var col = 0
        for ch in line {
            if scanningNumber {
                if ch.isNumber {
                    number += String(ch)
                } else {
                    numLocs.append((currentPos.0, currentPos.1, number))
                    number = ""
                    scanningNumber = false
                }
            } else {
                if ch.isNumber {
                    currentPos = (row, col)
                    number += String(ch)
                    scanningNumber = true
                }
            }
            col += 1
        }
        if scanningNumber {
            numLocs.append((currentPos.0, currentPos.1, number))
            number = ""
            scanningNumber = false
        }
        row += 1
    }
    return numLocs
}

func buildParts(contents: String) -> [(Int, Int, Character)] {
    var partLocs: [(Int, Int, Character)] = []
    var row = 0
    for line in contents.split(separator: "\n") {
        var col = 0
        for ch in line {
            if !ch.isNumber && ch != "." {
                partLocs.append((row, col, ch))
            }
            col += 1
        }
        row += 1
    }
    return partLocs
}

func checkParts(numLocs: [(Int, Int, String)], partLocs: [(Int, Int, Character)]) -> Int {
    var result = 0
    let neighbors = [ (-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]
    for partLoc in partLocs {
        let partRow = partLoc.0
        let partCol = partLoc.1
        var adjCount = 0
        var prod = 1
        for numLoc in numLocs {
            let numRow = numLoc.0
            let numColFirst = numLoc.1
            let number = numLoc.2
            let numColLast = numColFirst + number.count - 1
            var found = false
            for neighbor in neighbors {
                let adjPos = (partRow + neighbor.0, partCol + neighbor.1)
                for numCol in numColFirst...numColLast {
                    if (numRow == adjPos.0) && (numCol == adjPos.1) {
                        adjCount += 1
                        prod *= Int(number)!
                        found = true
                        break
                    }
                }
                if found {
                    break
                }
            }
        }
        if adjCount == 2 {
            result += prod
        }
    }
    return result
}

func process(contents: String) -> Int {
    let numLocs = buildNumbers(contents: contents)
    let partLocs = buildParts(contents: contents)
    return checkParts(numLocs: numLocs, partLocs: partLocs)
}

func main() {
    if CommandLine.arguments.count < 2 {
        usage()
    }
    let filename = CommandLine.arguments[1]
    guard let contents = try? String(contentsOfFile: filename, encoding: .utf8) else {
        let message = "error: could not read contents of file \(filename).\n"
        FileHandle.standardError.write(message.data(using: .utf8)!)
        exit(1)
    }
    let result = process(contents: contents)
    print("result = \(result)")
}

main()
