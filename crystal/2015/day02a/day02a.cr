#!/usr/bin/env ruby

def usage
    puts "usage: #{PROGRAM_NAME} <input file>"
    exit 1
end

def process(filename)
    total_area = 0
    File.each_line(filename) do |line|
        l, w, h = line.split("x").map(&.to_i)
        area1, area2, area3 = l * w, l * h, w * h
        surface_area = (2 * area1) + (2 * area2) + (2 * area3)
        min_area = [area1, area2, area3].min
        total_area += surface_area + min_area
    end
    total_area
end

def main
    usage if ARGV.size < 1
    filename = ARGV[0]
    result = process(filename)
    puts "result = #{result}"
end

main

