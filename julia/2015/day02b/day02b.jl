#!/usr/bin/env julia

function usage()
    progname = PROGRAM_FILE
    println(stderr, "usage: $progname <input file>")
    exit(1)
end

function process(content::String)::Int32
    total_len = 0
    for line in split(content, '\n', keepempty=false)
        dims = parse.(Int32, split(line, 'x'))
        l, w, h = dims
        perim1 = 2 * (l + w)
        perim2 = 2 * (l + h)
        perim3 = 2 * (w + h)
        present_len = minimum([perim1, perim2, perim3])
        bow_len = l * w * h
        total_len += present_len + bow_len
    end
    return Int32(total_len)
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
