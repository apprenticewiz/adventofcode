#!/usr/bin/env ruby

def usage
    progname = File.basename($0)
    puts "usage: #{progname} <input file>"
    exit 1
end

def process(filename)
    counter = 0
    File.foreach(filename) do |line|
        line.each_char do |ch|
            if ch == '('
                counter += 1
            elsif ch == ')'
                counter -= 1
            end
        end
    end
    counter
end

def main
    usage if ARGV.length < 1
    filename = ARGV[0]
    result = process(filename)
    puts "result = #{result}"
end

main if __FILE__ == $0
