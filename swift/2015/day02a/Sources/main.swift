import Foundation

func usage() {
    FileHandle.standardError.write("usage: swift run <input file>\n".data(using: .utf8)!)
    exit(1)
}

func process(filename: String) -> Int {
    var totalArea = 0

    do {
        let content = try String(contentsOfFile: filename, encoding: .utf8)
        let lines = content.split(separator: "\n")

        for line in lines {
            let tokens = line.split(separator: "x")
            guard tokens.count == 3 else { continue }

            let dimensions = tokens.compactMap { Int($0) }
            guard dimensions.count == 3 else { continue }

            let (l, w, h) = (dimensions[0], dimensions[1], dimensions[2])
            let area1 = l * w
            let area2 = l * h
            let area3 = w * h
            let surfaceArea = 2 * area1 + 2 * area2 + 2 * area3
            let minArea = min(area1, area2, area3)

            totalArea += surfaceArea + minArea
        }
    } catch {
        FileHandle.standardError.write("Error reading file: \(error.localizedDescription)\n".data(using: .utf8)!)
        exit(1)
    }

    return totalArea
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
