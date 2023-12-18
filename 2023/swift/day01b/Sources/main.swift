import Foundation

func usage() {
    let progname = (CommandLine.arguments[0] as NSString).lastPathComponent
    print("usage: swift run \(progname) <file>")
    exit(1)
}

func process(contents: String) -> Int {
    let lines = contents.split(separator: "\n")
    let digits = ["0": "0", "1": "1", "2": "2", "3": "3", "4": "4", "5": "5",
        "6": "6", "7": "7", "8": "8", "9": "9", "zero": "0", "one": "1",
        "two": "2", "three": "3", "four": "4", "five": "5", "six": "6",
        "seven": "7", "eight": "8", "nine": "9"]
    var sum = 0
    for line in lines {
        var maxIndex = -1
        var minIndex = line.utf8.count
        var leftDigit = ""
        var rightDigit = ""
        for digit in digits.keys {
            if line.contains(digit) {
                var range = line.range(of: digit)!
                let leftIndex = line.distance(from: line.startIndex, to: range.lowerBound)
                if leftIndex < minIndex {
                    minIndex = leftIndex
                    leftDigit = digits[digit]!
                }
                range = line.range(of: digit, options: .backwards)!
                let rightIndex = line.distance(from: line.startIndex, to: range.lowerBound)
                if rightIndex > maxIndex {
                    maxIndex = rightIndex
                    rightDigit = digits[digit]!
                }
            }
        }
        sum += Int(leftDigit + rightDigit)!
    }
    return sum
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
