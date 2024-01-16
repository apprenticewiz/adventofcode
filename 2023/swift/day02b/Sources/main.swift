import Foundation

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
        var redNeeded = 0
        var greenNeeded = 0
        var blueNeeded = 0
        for draw in draws.split(separator: "; ") {
            for colorAmount in draw.split(separator: ", ") {
                let amount = Int(colorAmount.split(separator: " ")[0])!
                let color = colorAmount.split(separator: " ")[1]
                switch color {
                    case "red":
                        if ( amount > redNeeded ) {
                            redNeeded = amount
                        }
                    case "green":
                        if ( amount > greenNeeded ) {
                            greenNeeded = amount
                        }
                    case "blue":
                        if ( amount > blueNeeded ) {
                            blueNeeded = amount
                        }
                    default:
                        fatalError("unexpected color")
                }
            }
        }
        result += redNeeded * greenNeeded * blueNeeded
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
