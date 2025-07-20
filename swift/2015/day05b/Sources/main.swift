import Foundation

func usage() {
    FileHandle.standardError.write("usage: swift run <input file>\n".data(using: .utf8)!)
    exit(1)
}

func prop1(_ str: String) -> Bool {
    let chars = Array(str)
    if chars.count < 4 { return false }
    for i in 0..<chars.count - 3 {
        let firstPair = String(chars[i...i+1])
        for j in i + 2..<chars.count - 1 {
            let secondPair = String(chars[j...j+1])
            if firstPair == secondPair {
                return true
            }
        }
    }
    return false
}

func prop2(_ str: String) -> Bool {
    let chars = Array(str)
    for i in 0..<chars.count - 2 {
        if chars[i] == chars[i + 2] {
            return true;
        }
    }
    return false;
}

func process(filename: String) -> Int {
    var count = 0

    do {
        let content = try String(contentsOfFile: filename, encoding: .utf8)
        let lines = content.components(separatedBy: .newlines)
        for line in lines {
            if prop1(line) && prop2(line) {
                count += 1
            }
        }
    } catch {
        FileHandle.standardError.write("Error reading file: \(error.localizedDescription)\n".data(using: .utf8)!)
        exit(1)
    }

    return count
}

func main() {
    let args = CommandLine.arguments
    if args.count < 2 {
        usage()
    }

    let filename = args[1]
    let result = process(filename: filename)
    print("result = \(result)")
}

main()
