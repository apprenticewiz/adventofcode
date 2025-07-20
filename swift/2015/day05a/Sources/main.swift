import Foundation

func usage() {
    FileHandle.standardError.write("usage: swift run <input file>\n".data(using: .utf8)!)
    exit(1)
}

func prop1(_ str: String) -> Bool {
    let vowels = str.filter { "aeiou".contains($0) }.count
    return vowels >= 3
}

func prop2(_ str: String) -> Bool {
    let chars = Array(str)
    for i in 0..<chars.count - 1 {
        if chars[i] == chars[i + 1] {
            return true;
        }
    }
    return false;
}

func prop3(_ str: String) -> Bool {
    return !str.contains("ab") &&
        !str.contains("cd") &&
        !str.contains("pq") &&
        !str.contains("xy")
}

func process(filename: String) -> Int {
    var count = 0

    do {
        let content = try String(contentsOfFile: filename, encoding: .utf8)
        let lines = content.components(separatedBy: .newlines)
        for line in lines {
            if prop1(line) && prop2(line) && prop3(line) {
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
