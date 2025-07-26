#!/usr/bin/env ruby

require 'digest'

def usage
    progname = File.basename($0)
    puts "usage: #{progname} <key>"
    exit 1
end

def process(key)
    n = 1
    while true
        try_key = key + n.to_s
        digest = Digest::MD5.hexdigest(try_key)
        if digest.start_with?("000000")
            return n
        else
            n += 1
        end
    end
end

def main
    usage if ARGV.length < 1
    key = ARGV[0]
    result = process(key)
    puts "result = #{result}"
end

main if __FILE__ == $0
