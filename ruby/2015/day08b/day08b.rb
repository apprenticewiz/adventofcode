#!/usr/bin/env ruby

def usage
    progname = File.basename($0)
    puts "usage: #{progname} <input file>"
    exit 1
end

def process(filename)
    result = 0
    File.foreach(filename) do |line|
        code_len = line.length
        enc_len = 0
        for i in 0..(line.length - 1) do
            case line[i]
            when "\\", "\""
                enc_len += 2
            else
                enc_len += 1
            end
        end
        result += 2 + (enc_len - code_len)
    end
    result
end

def main
    usage if ARGV.length < 1
    filename = ARGV[0]
    result = process(filename)
    puts "result = #{result}"
end

main if __FILE__ == $0
