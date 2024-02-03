import Foundation

func usage() {
    let progname = (CommandLine.arguments[0] as NSString).lastPathComponent
    print("usage: swift run \(progname) <file>")
    exit(1)
}

func process(contents: String) -> Int {
    let lines = contents.split(separator: "\n")
    var result = 0
    var instances = Dictionary<Int, Int>()
    for line in lines {
        let cardStr = line.split(separator: ": ")[0]
        let cardNumber = Int(cardStr.split(separator: " ")[1])!
        let rest = line.split(separator: ": ")[1]
        let winningStr = rest.split(separator: " | ")[0]
        var winningSet = Set<Int>()
        for winningNum in winningStr.split(separator: " ") {
            winningSet.insert(Int(winningNum)!)
        }
        let handStr = rest.split(separator: " | ")[1]
        var handSet = Set<Int>()
        for handNum in handStr.split(separator: " ") {
            handSet.insert(Int(handNum)!)
        }
        let intersection = winningSet.intersection(handSet)
        result += 1
        if (cardNumber + 1) <= (cardNumber + intersection.count) {
            for i in (cardNumber + 1)...(cardNumber + intersection.count) {
                let copies = (instances[i] ?? 0) + 1 + (instances[cardNumber] ?? 0)
                instances[i] = copies
            }
        }
    }
    for i in instances.keys {
        result += instances[i]!
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
