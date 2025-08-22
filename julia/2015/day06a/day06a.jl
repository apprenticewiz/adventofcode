#!/usr/bin/env julia

struct Position
  x :: Int32
  y :: Int32
end

struct Bounds
  upperLeft :: Position
  lowerRight :: Position
end

function usage()
    progname = PROGRAM_FILE
    println(stderr, "usage: $progname <input file>")
    exit(1)
end

function perform(grid::BitMatrix, action::String, bounds::Bounds)
    for row in bounds.upperLeft.y:bounds.lowerRight.y
        for col in bounds.upperLeft.x:bounds.lowerRight.x
            if action == "turn on"
              grid[row, col] = true
            elseif action == "turn off"
              grid[row, col] = false
            elseif action == "toggle"
              grid[row, col] = !grid[row, col]
            end
        end
    end
end

function process(content::String)::Int32
    grid = falses(1000, 1000)
    for line in split(content, '\n', keepempty=false)
        matches = match(r"(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)", line)
        if matches != nothing
            action :: String = matches.captures[1]
            bounds = Bounds(Position(parse(Int32, matches.captures[2]) + 1,
                                     parse(Int32, matches.captures[3]) + 1),
                            Position(parse(Int32, matches.captures[4]) + 1,
                                     parse(Int32, matches.captures[5]) + 1))
            perform(grid, action, bounds)
        end
    end
    return count(==(true), grid)
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
