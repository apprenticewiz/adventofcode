import Foundation

import AOC_Utils

struct Bounds {
    var upperLeft: Geometry.Position2D<Int>
    var lowerRight: Geometry.Position2D<Int>
}

class Grid {
    var grid: [[Int]]
    static let ROW_MAX = 1000
    static let COL_MAX = 1000

    init() {
        grid = Array(
            repeating: Array(repeating: 0, count: Grid.COL_MAX),
            count: Grid.ROW_MAX
        )
    }

    func perform(action: String, bounds: Bounds) {
        for row in bounds.upperLeft.x...bounds.lowerRight.x {
            for col in bounds.upperLeft.y...bounds.lowerRight.y {
                switch action {
                case "turn on":
                    grid[row][col] += 1
                case "turn off":
                    grid[row][col] = max(0, grid[row][col] - 1)
                case "toggle":
                    grid[row][col] += 2
                default:
                    break
                }
            }
        }
    }

    func count() -> Int {
        var total = 0
        for row in 0..<Grid.ROW_MAX {
            for col in 0..<Grid.COL_MAX {
                total += grid[row][col]
            }
        }
        return total
    }
}

func usage() {
    FileHandle.standardError.write("usage: swift run <input file>\n".data(using: .utf8)!)
    exit(1)
}

func process(filename: String) -> Int {
    let grid = Grid()
    let regex = try! NSRegularExpression(
        pattern: "(turn on|turn off|toggle) (\\d+),(\\d+) through (\\d+),(\\d+)"
    )

    do {
        let content = try String(contentsOfFile: filename, encoding: .utf8)
        for line in content.split(separator: "\n", omittingEmptySubsequences: false) {
            let lineStr = String(line)
            let range = NSRange(lineStr.startIndex..<lineStr.endIndex, in: lineStr)
            if let match = regex.firstMatch(in: lineStr, range:range) {
                let action = lineStr.substring(with: match.range(at: 1))
                let r1 = Int(lineStr.substring(with: match.range(at: 2)))!
                let c1 = Int(lineStr.substring(with: match.range(at: 3)))!
                let r2 = Int(lineStr.substring(with: match.range(at: 4)))!
                let c2 = Int(lineStr.substring(with: match.range(at: 5)))!
                let bounds = Bounds(
                    upperLeft: Geometry.Position2D(r1, c1),
                    lowerRight: Geometry.Position2D(r2, c2)
                )
                grid.perform(action: action, bounds: bounds)
            }
        }
    } catch {
        FileHandle.standardError.write("Error reading file: \(error.localizedDescription)\n".data(using: .utf8)!)
        exit(1)
    }

    return grid.count()
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
