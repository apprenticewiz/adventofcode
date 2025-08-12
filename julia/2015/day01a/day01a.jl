#!/usr/bin/env julia

function usage()
    progname = PROGRAM_FILE
    println(stderr, "usage: $progname <input file>")
    exit(1)
end

function process(content::String)::Int32
    floor = 0
    for c in content
        if c == '('
            floor += 1
        elseif c == ')'
            floor -= 1
        end
    end
    return Int32(floor)
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
