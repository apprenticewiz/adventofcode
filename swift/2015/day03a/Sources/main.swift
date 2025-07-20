import Foundation

import AOC_Utils

func usage() {
    FileHandle.standardError.write("usage: swift run <input file>\n".data(using: .utf8)!)
    exit(1)
}

func process(filename: String) -> Int {
    var positions = Set<Geometry.Position2D<Int>>()
    var santa = Geometry.Position2D<Int>(0, 0)
    positions.insert(santa)

    do {
        let content = try String(contentsOfFile: filename, encoding: .utf8)
        for line in content.split(separator: "\n", omittingEmptySubsequences: false) {
            for ch in line {
                switch ch {
                case "^":
                    santa.y += 1
                case "v":
                    santa.y -= 1
                case "<":
                    santa.x -= 1
                case ">":
                    santa.x += 1
                default:
                    continue
                }
                positions.insert(santa)
            }
        }
    } catch {
        FileHandle.standardError.write("Error reading file: \(error.localizedDescription)\n".data(using: .utf8)!)
        exit(1)
    }

    return positions.count
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
