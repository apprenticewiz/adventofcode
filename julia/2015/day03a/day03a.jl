#!/usr/bin/env julia

function usage()
    progname = PROGRAM_FILE
    println(stderr, "usage: $progname <input file>")
    exit(1)
end

function process(content::String)::Int32
    santa = (0, 0)
    positions = Set{Tuple{Int32, Int32}}()
    push!(positions, santa)
    for c in content
        if c == '^'
            santa = (santa[1], santa[2] + 1)
        elseif c == 'v'
            santa = (santa[1], santa[2] - 1)
        elseif c == '<'
            santa = (santa[1] - 1, santa[2])
        elseif c == '>'
            santa = (santa[1] + 1, santa[2])
        end
        push!(positions, santa)
    end
    return Int32(length(positions))
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
