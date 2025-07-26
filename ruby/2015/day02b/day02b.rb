#!/usr/bin/env ruby

def usage
    progname = File.basename($0)
    puts "usage: #{progname} <input file>"
    exit 1
end

def process(filename)
    total_len = 0
    File.foreach(filename) do |line|
        l, w, h = line.split("x").map(&:to_i)
        perim1 = 2 * (l + w)
        perim2 = 2 * (l + h)
        perim3 = 2 * (w + h)
        present_len = [perim1, perim2, perim3].min
        bow_len = l * w * h
        total_len += present_len + bow_len
    end
    total_len
end

def main
    usage if ARGV.length < 1
    filename = ARGV[0]
    result = process(filename)
    puts "result = #{result}"
end

main if __FILE__ == $0
