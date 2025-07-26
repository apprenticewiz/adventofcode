#!/usr/bin/env ruby

require 'set'

def usage
    progname = File.basename($0)
    puts "usage: #{progname} <input file>"
    exit 1
end

def process(filename)
    santa = [0, 0]
    positions = Set.new()
    positions.add(santa)
    File.foreach(filename) do |line|
        line.each_char do |ch|
            case ch
            when '^'
                santa = [santa[0], santa[1] + 1]
            when 'v'
                santa = [santa[0], santa[1] - 1]
            when '<'
                santa = [santa[0] - 1, santa[1]]
            when '>'
                santa = [santa[0] + 1, santa[1]]
            end
            positions.add(santa)
        end
    end
    positions.size
end

def main
    usage if ARGV.length < 1
    filename = ARGV[0]
    result = process(filename)
    puts "result = #{result}"
end

main if __FILE__ == $0
