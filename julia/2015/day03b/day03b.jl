#!/usr/bin/env julia

function usage()
    progname = PROGRAM_FILE
    println(stderr, "usage: $progname <input file>")
    exit(1)
end

function process(content::String)::Int32
    santa = (0, 0)
    robo_santa = (0, 0)
    positions = Set{Tuple{Int32, Int32}}()
    push!(positions, santa)
    santa_move = false
    for c in content
        if c == '^'
            santa_move ? santa = (santa[1], santa[2] + 1) : robo_santa = (robo_santa[1], robo_santa[2] + 1)
        elseif c == 'v'
            santa_move ? santa = (santa[1], santa[2] - 1) : robo_santa = (robo_santa[1], robo_santa[2] - 1)
        elseif c == '<'
            santa_move ? santa = (santa[1] - 1, santa[2]) : robo_santa = (robo_santa[1] - 1, robo_santa[2])
        elseif c == '>'
            santa_move ? santa = (santa[1] + 1, santa[2]) : robo_santa = (robo_santa[1] + 1, robo_santa[2])
        end
	santa_move ? push!(positions, santa) : push!(positions, robo_santa)
        santa_move = !santa_move
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
