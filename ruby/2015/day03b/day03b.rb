#!/usr/bin/env ruby

require 'set'

def usage
    progname = File.basename($0)
    puts "usage: #{progname} <input file>"
    exit 1
end

def process(filename)
    santa = [0, 0]
    robo_santa = [0, 0]
    positions = Set.new()
    positions.add(santa)
    i = 0
    File.foreach(filename) do |line|
        line.each_char do |ch|
            santa_move = (i % 2) == 0
            case ch
            when '^'
                santa_move ? santa = [santa[0], santa[1] + 1] : robo_santa = [robo_santa[0], robo_santa[1] + 1]
            when 'v'
                santa_move ? santa = [santa[0], santa[1] - 1] : robo_santa = [robo_santa[0], robo_santa[1] - 1]
            when '<'
                santa_move ? santa = [santa[0] - 1, santa[1]] : robo_santa = [robo_santa[0] - 1, robo_santa[1]]
            when '>'
                santa_move ? santa = [santa[0] + 1, santa[1]] : robo_santa = [robo_santa[0] + 1, robo_santa[1]]
            end
            santa_move ? positions.add(santa) : positions.add(robo_santa)
            i += 1
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
