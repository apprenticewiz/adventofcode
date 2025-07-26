#!/usr/bin/env ruby

def usage
    progname = File.basename($0)
    puts "usage: #{progname} <input file>"
    exit 1
end

def process(filename)
    total_area = 0
    File.foreach(filename) do |line|
        l, w, h = line.split("x").map(&:to_i)
        area1 = l * w
        area2 = l * h
        area3 = w * h
        surface_area = (2 * area1) + (2 * area2) + (2 * area3)
        min_area = [area1, area2, area3].min
        total_area += surface_area + min_area
    end
    total_area
end

def main
    usage if ARGV.length < 1
    filename = ARGV[0]
    result = process(filename)
    puts "result = #{result}"
end

main if __FILE__ == $0
