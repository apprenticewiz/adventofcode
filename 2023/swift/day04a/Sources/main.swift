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
        result += (intersection.count > 0) ? 1 << (intersection.count - 1) : 0
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
