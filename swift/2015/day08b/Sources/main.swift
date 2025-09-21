import Foundation

func usage() {
    FileHandle.standardError.write("usage: swift run <input file>\n".data(using: .utf8)!)
    exit(1)
}

func process(filename: String) -> Int {
    var result = 0

    do {
        let content = try String(contentsOfFile: filename, encoding: .utf8)
        let lines = content.components(separatedBy: .newlines).filter { !$0.isEmpty }
        for line in lines {
            let codeLen = line.utf8.count
            var encLen = 0
            let lineChars = Array(line.utf8)
            lineChars.forEach { ch in
                switch ( ch ) {
                case 34, 92:
                    encLen += 2
                default:
                    encLen += 1
                }
            }
            result += 2 + (encLen - codeLen)
        }
    } catch {
        FileHandle.standardError.write("Error reading file: \(error.localizedDescription)\n".data(using: .utf8)!)
        exit(1)
    }

    return result
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
