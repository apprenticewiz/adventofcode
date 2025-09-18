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
        memLen = 0
        i = 2
        while i < length(line)
            if line[i] == '\\'
                if line[i + 1] == '\\' || line[i + 1] == '\"'
                    i += 2
                elseif line[i + 1] == 'x'
                    i += 4
                else
                    i += 1
                end
            else
                i += 1
            end
            memLen += 1
        end
        res += codeLen - memLen
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
