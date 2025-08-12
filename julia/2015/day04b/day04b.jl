#!/usr/bin/env julia

using MD5

function usage()
    progname = PROGRAM_FILE
    println(stderr, "usage: $progname <key>")
    exit(1)
end

function process(key::String)::Int32
    n = 1
    while true
        try_key = key * string(n)
        digest = md5(try_key)
        hex_digest = bytes2hex(digest)
        if startswith(hex_digest, "000000")
            return Int32(n)
        else
            n += 1
        end
    end
end

function main()
    args = ARGS
    if length(args) < 1
        usage()
    end
    key = args[1]
    result = process(key)
    println("result = $result")
end

if abspath(PROGRAM_FILE) == @__FILE__
    main()
end
