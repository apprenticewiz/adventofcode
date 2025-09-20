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
        mem_len = 0
        i = 1
        while i < line.length - 1 do
            case line[i]
            when "\\"
                case line[i + 1]
                when "\\", "\""
                    i += 2
                when "x"
                    i += 4
                else
                    i += 1
                end
            else
                i += 1
            end
            mem_len += 1
        end
        result += code_len - mem_len
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
