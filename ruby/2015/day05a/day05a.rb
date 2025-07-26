#!/usr/bin/env ruby

require 'set'

def usage
    progname = File.basename($0)
    puts "usage: #{progname} <input file>"
    exit 1
end

def prop1?(str)
    vowels = str.scan(/[aeiou]/)
    vowels.length >= 3
end

def prop2?(str)
    !!(str =~ /(.)\1/)
end

def prop3?(str)
    !(str =~ /(ab|cd|pq|xy)/)
end

def process(filename)
    counter = 0
    File.foreach(filename) do |line|
        counter += 1 if prop1?(line) && prop2?(line) && prop3?(line)
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
