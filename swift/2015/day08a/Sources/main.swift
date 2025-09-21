import Foundation

func usage() {
    FileHandle.standardError.write("usage: swift run <input file>\n".data(using: .utf8)!)
    exit(1)
}

func process(filename: String) -> Int {
    var result = 0

    do {
        let content = try String(contentsOfFile: filename, encoding: .utf8)
        let lines = content.components(separatedBy: .newlines)
        for line in lines {
            let codeLen = line.utf8.count
            var memLen = 0
            var i = 1
            let lineChars = Array(line.utf8)
            while ( i < codeLen - 1 ) {
                switch lineChars[i] {
                case 92:
                    switch lineChars[i + 1] {
                    case 34, 92:
                        i += 2
                    case 120:
                        i += 4
                    default:
                        i += 1
                    }
                default:
                    i += 1
                }
                memLen += 1
            }
            result += codeLen - memLen
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
