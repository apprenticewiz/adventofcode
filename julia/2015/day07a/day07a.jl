#!/usr/bin/env julia

struct Assign
    src :: String
end

struct Not
    src :: String
end

struct And
    src1 :: String
    src2 :: String
end

struct Or
    src1 :: String
    src2 :: String
end

struct LeftShift
    src :: String
    amt :: Int32
end

struct RightShift
    src :: String
    amt :: Int32
end

const Operation = Union{Assign, Not, And, Or, LeftShift, RightShift}

function usage()
    progname = PROGRAM_FILE
    println(stderr, "usage: $progname <input file>")
    exit(1)
end

function process(content::String)::Int32
    operations = Dict{String, Operation}()
    cache = Dict{String, Int32}()
    src1 :: String = ""
    src2 :: String = ""
    amt :: Int32 = 0
    dest :: String = ""
    for line in split(content, '\n', keepempty=false)
        matches = match(r"^(\d+|\w+) -> (\w+)$", line)
        if matches != nothing
            src1 = matches.captures[1]
            dest = matches.captures[2]
            operations[dest] = Assign(src1)
        end
        matches = match(r"NOT (\d+|\w+) -> (\w+)", line)
        if matches != nothing
            src1 = matches.captures[1]
            dest = matches.captures[2]
            operations[dest] = Not(src1)
        end
        matches = match(r"(\d+|\w+) (AND|OR) (\d+|\w+) -> (\w+)", line)
        if matches != nothing
            src1 = matches.captures[1]
            op = matches.captures[2]
            src2 = matches.captures[3]
            dest = matches.captures[4]
            if op == "AND"
                operations[dest] = And(src1, src2)
            else
                operations[dest] = Or(src1, src2)
            end
        end
        matches = match(r"(\d+|\w+) (LSHIFT|RSHIFT) (\d+) -> (\w+)", line)
        if matches != nothing
            src1 = matches.captures[1]
            op = matches.captures[2]
            amt = parse(Int32, matches.captures[3])
            dest = matches.captures[4]
            if op == "LSHIFT"
                operations[dest] = LeftShift(src1, amt)
            else
                operations[dest] = RightShift(src1, amt)
            end
        end
    end
    return eval(operations, cache, "a")
end

function eval(ops, cache, expr)
    if tryparse(Int32, expr) !== nothing
        return parse(Int32, expr)
    elseif haskey(cache, expr)
        return cache[expr]
    else
        r = 0
        op = ops[expr]
        if op isa Assign
            r = eval(ops, cache, op.src)
        elseif op isa Not
            a = eval(ops, cache, op.src)
            r = ~a
        elseif op isa And
            a = eval(ops, cache, op.src1)
            b = eval(ops, cache, op.src2)
            r = a & b
        elseif op isa Or
            a = eval(ops, cache, op.src1)
            b = eval(ops, cache, op.src2)
            r = a | b
        elseif op isa LeftShift
            a = eval(ops, cache, op.src)
            r = a << op.amt
        elseif op isa RightShift
            a = eval(ops, cache, op.src)
            r = a >> op.amt
        end
        masked = r & 0xffff
        cache[expr] = masked
        return masked
    end
end

function main()
    args = ARGS
    if length(args) < 1
        usage()
    end
    filename = args[1]
    try
        content = read(filename, String)
        result = process(content)
        println("result = $result")
    catch e
        println(stderr, "error reading file: $e")
        exit(1)
    end
end

if abspath(PROGRAM_FILE) == @__FILE__
    main()
end
