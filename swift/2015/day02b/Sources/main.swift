import Foundation

func usage() {
    FileHandle.standardError.write("usage: swift run <input file>\n".data(using: .utf8)!)
    exit(1)
}

func process(filename: String) -> Int {
    var totalLength = 0

    do {
        let content = try String(contentsOfFile: filename, encoding: .utf8)
        let lines = content.split(separator: "\n")

        for line in lines {
            let tokens = line.split(separator: "x")
            guard tokens.count == 3 else { continue }

            let dimensions = tokens.compactMap { Int($0) }
            guard dimensions.count == 3 else { continue }

            let (l, w, h) = (dimensions[0], dimensions[1], dimensions[2])
            let perim1 = 2 * (l + w)
            let perim2 = 2 * (l + h)
            let perim3 = 2 * (w + h)
            let presentLength = min(perim1, perim2, perim3)
            let bowLength = l * w * h

            totalLength += presentLength + bowLength
        }
    } catch {
        FileHandle.standardError.write("Error reading file: \(error.localizedDescription)\n".data(using: .utf8)!)
        exit(1)
    }

    return totalLength
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
