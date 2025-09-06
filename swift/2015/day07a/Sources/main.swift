import Foundation

enum Operation {
    case assign(String)
    case not(String)
    case and(String, String)
    case or(String, String)
    case leftShift(String, Int)
    case rightShift(String, Int)
}

func usage() {
    FileHandle.standardError.write("usage: swift run <input file>\n".data(using: .utf8)!)
    exit(1)
}

func process(filename: String) -> Int {
    var operations: [String: Operation] = [:]
    var cache: [String: Int] = [:]
    let re1 = try! NSRegularExpression(pattern: "^(\\d+|\\w+) -> (\\w+)$")
    let re2 = try! NSRegularExpression(pattern: "NOT (\\d+|\\w+) -> (\\w+)")
    let re3 = try! NSRegularExpression(pattern: "(\\d+|\\w+) (AND|OR) (\\d+|\\w+) -> (\\w+)")
    let re4 = try! NSRegularExpression(pattern: "(\\d+|\\w+) (LSHIFT|RSHIFT) (\\d+) -> (\\w+)")

    do {
        let content = try String(contentsOfFile: filename, encoding: .utf8)
        for line in content.split(separator: "\n", omittingEmptySubsequences: false) {
            let lineStr = String(line)
            let range = NSRange(lineStr.startIndex..<lineStr.endIndex, in: lineStr)
            if let match = re1.firstMatch(in: lineStr, range: range) {
                let src = lineStr.substring(with: match.range(at: 1))
                let dest = lineStr.substring(with: match.range(at: 2))
                operations[dest] = Operation.assign(src)
            } else if let match = re2.firstMatch(in: lineStr, range: range) {
                let src = lineStr.substring(with: match.range(at: 1))
                let dest = lineStr.substring(with: match.range(at: 2))
                operations[dest] = Operation.not(src)
            } else if let match = re3.firstMatch(in: lineStr, range: range) {
                let src1 = lineStr.substring(with: match.range(at: 1))
                let op = lineStr.substring(with: match.range(at: 2))
                let src2 = lineStr.substring(with: match.range(at: 3))
                let dest = lineStr.substring(with: match.range(at: 4))
                if op == "AND" {
                    operations[dest] = Operation.and(src1, src2)
                } else {
                    operations[dest] = Operation.or(src1, src2)
                }
            } else if let match = re4.firstMatch(in: lineStr, range: range) {
                let src = lineStr.substring(with: match.range(at: 1))
                let op = lineStr.substring(with: match.range(at: 2))
                let amt = Int(lineStr.substring(with: match.range(at: 3)))!
                let dest = lineStr.substring(with: match.range(at: 4))
                if op == "LSHIFT" {
                    operations[dest] = Operation.leftShift(src, amt)
                } else {
                    operations[dest] = Operation.rightShift(src, amt)
                }
            }
        }
    } catch {
        FileHandle.standardError.write("Error reading file: \(error.localizedDescription)\n".data(using: .utf8)!)
        exit(1)
    }

    return eval(ops: operations, cache: &cache, expr: "a")
}

func eval(ops: [String: Operation], cache: inout [String: Int], expr: String) -> Int {
    if let _ = Int(expr) {
        return Int(expr)!
    }
    if cache.keys.contains(expr) {
        return cache[expr]!
    }
    var r = 0
    switch ops[expr] {
        case .assign(let src):
            let a = eval(ops: ops, cache: &cache, expr: src)
            r = a
        case .not(let src):
            let a = eval(ops: ops, cache: &cache, expr: src)
            r = ~a
        case .and(let src1, let src2):
            let a = eval(ops: ops, cache: &cache, expr: src1)
            let b = eval(ops: ops, cache: &cache, expr: src2)
            r = a & b
        case .or(let src1, let src2):
            let a = eval(ops: ops, cache: &cache, expr: src1)
            let b = eval(ops: ops, cache: &cache, expr: src2)
            r = a | b
        case .leftShift(let src, let amt):
            let a = eval(ops: ops, cache: &cache, expr: src)
            r = a << amt
        case .rightShift(let src, let amt):
            let a = eval(ops: ops, cache: &cache, expr: src)
            r = a >> amt
        case .none:
            r = 0
    }
    let masked = r & 0xffff
    cache[expr] = masked
    return masked;
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
