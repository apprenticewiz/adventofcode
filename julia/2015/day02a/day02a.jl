#!/usr/bin/env julia

function usage()
    progname = PROGRAM_FILE
    println(stderr, "usage: $progname <input file>")
    exit(1)
end

function process(content::String)::Int32
    total_area = 0
    for line in split(content, '\n', keepempty=false)
        dims = parse.(Int32, split(line, 'x'))
        l, w, h = dims
        area1 = l * w
        area2 = l * h
        area3 = w * h
        surface_area = 2 * area1 + 2 * area2 + 2 * area3
        min_area = minimum([area1, area2, area3])
        total_area += surface_area + min_area
    end
    return Int32(total_area)
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
