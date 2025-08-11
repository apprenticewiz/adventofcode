#!/usr/bin/env ruby

def usage
    puts "usage: #{PROGRAM_NAME} <input file>"
    exit 1
end

def process(filename)
    counter = 0
    File.each_line(filename) do |line|
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
    usage if ARGV.size < 1
    filename = ARGV[0]
    result = process(filename)
    puts "result = #{result}"
end

main

