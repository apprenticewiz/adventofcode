#!/usr/bin/env ruby

require 'set'

def usage
    progname = File.basename($0)
    puts "usage: #{progname} <input file>"
    exit 1
end

def perform(grid, action, r1, c1, r2, c2)
    for row in r1..r2 do
        for col in c1..c2 do
            case action
                when "turn on"
                    grid[row*1000 + col] = true
                when "turn off"
                    grid[row*1000 + col] = false
                when "toggle"
                    grid[row*1000 + col] = !grid[row*1000 + col]
            end
        end
    end
end

def count(grid)
    total = 0
    for row in 0..999 do
        for col in 0..999 do
            if grid[row*1000 + col] then
                total += 1
            end
        end
    end
    total
end

def process(filename)
    grid = Array.new(1000*1000, false)
    re = /(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)/
    File.foreach(filename) do |line|
        matches = re.match(line)
        action = matches[1]
        r1 = matches[2].to_i
        c1 = matches[3].to_i
        r2 = matches[4].to_i
        c2 = matches[5].to_i
        perform(grid, action, r1, c1, r2, c2)
    end
    count(grid)
end

def main
    usage if ARGV.length < 1
    filename = ARGV[0]
    result = process(filename)
    puts "result = #{result}"
end

main if __FILE__ == $0
