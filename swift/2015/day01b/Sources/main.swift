import Foundation

func usage() {
    FileHandle.standardError.write("usage: swift run <input file>\n".data(using: .utf8)!)
    exit(1)
}

func process(filename: String) -> Int {
    var counter = 0
    var pos = 0

    do {
        let content = try String(contentsOfFile: filename, encoding: .utf8)
        for line in content.split(separator: "\n", omittingEmptySubsequences: false) {
            for ch in line {
                pos += 1
                switch ch {
                case "(":
                    counter += 1
                case ")":
                    counter -= 1
                default:
                    continue
                }
                if ( counter < 0 ) {
                    return pos
                }
            }
        }
    } catch {
        FileHandle.standardError.write("Error reading file: \(error.localizedDescription)\n".data(using: .utf8)!)
        exit(1)
    }

    return 0
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
