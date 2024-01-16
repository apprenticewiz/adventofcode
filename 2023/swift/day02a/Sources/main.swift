import Foundation

let totalRed = 12
let totalGreen = 13
let totalBlue = 14

func usage() {
    let progname = (CommandLine.arguments[0] as NSString).lastPathComponent
    print("usage: swift run \(progname) <file>")
    exit(1)
}

func process(contents: String) -> Int {
    let lines = contents.split(separator: "\n")
    var result = 0
    for line in lines {
        let gamePart = line.split(separator: ": ")[0]
        let gameNum = Int(gamePart.split(separator: " ")[1])!
        let draws = line.split(separator: ": ")[1]
        var valid = true
        for draw in draws.split(separator: "; ") {
            for colorAmount in draw.split(separator: ", ") {
                let amount = Int(colorAmount.split(separator: " ")[0])!
                let color = colorAmount.split(separator: " ")[1]
                switch color {
                    case "red":
                        if ( amount > totalRed ) {
                            valid = false
                        }
                    case "green":
                        if ( amount > totalGreen ) {
                            valid = false
                        }
                    case "blue":
                        if ( amount > totalBlue ) {
                            valid = false
                        }
                    default:
                        fatalError("unexpected color")
                }
            }
        }
        if ( valid ) {
            result += gameNum
        }
    }
    return result
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
