#!/usr/bin/env julia

function usage()
    progname = PROGRAM_FILE
    println(stderr, "usage: $progname <input file>")
    exit(1)
end

function process(content::String)::Int32
    res = 0
    for line in split(content, '\n', keepempty=false)
        codeLen = length(line)
        encLen = 0
        for i in 1:(length(line))
            if line[i] == '\\' || line[i] == '\"'
                encLen += 2
            else
                encLen += 1
            end
        end
        res += 2 + (encLen - codeLen)
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
