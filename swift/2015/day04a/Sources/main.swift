import Foundation

import AOC_Utils

func usage() {
    FileHandle.standardError.write("usage: swift run <key>\n".data(using: .utf8)!)
    exit(1)
}

func process(key: String) -> Int {
    var n = 1

    while true {
        let tryKey = key + String(n)
        let digest = md5(tryKey)

        if digest.hasPrefix("00000") {
            break
        }
        n += 1
    }

    return n        
}

func main() {
    let args = CommandLine.arguments
    if args.count < 2 {
        usage()
    }

    let key = args[1]
    let result = process(key: key)
    print("result = \(result)")
}

main()
