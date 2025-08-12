#!/usr/bin/env julia

function usage()
    progname = PROGRAM_FILE
    println(stderr, "usage: $progname <input file>")
    exit(1)
end

function prop1(line::String)::Bool
    return count(c -> c in "aeiou", line) >= 3
end

function prop2(line::String)::Bool
    return occursin(r"(.)\1", line)
end

function prop3(line::String)::Bool
    return !occursin(r"(ab|cd|pq|xy)", line)
end

function process(content::String)::Int32
    res = 0
    for line in split(content, '\n', keepempty=false)
        if prop1(String(line)) && prop2(String(line)) && prop3(String(line))
            res += 1
        end
    end
    return Int32(res)
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
