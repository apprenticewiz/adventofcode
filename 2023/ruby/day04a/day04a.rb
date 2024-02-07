#!/usr/bin/env ruby

require 'set'

def usage
    progname = $PROGRAM_NAME
    puts "usage: #{progname} <file>"
    exit(1)
  end
  
def process(contents)
    result = 0
    contents.each_line do |line|
        rest = line.split(%r{:\s+})[1]
        winning_str = rest.split(%r{\s+\|\s+})[0]
        hand_str = rest.split(%r{\s+\|\s+})[1]
        winning_set = Set.new(winning_str.split().map(&:to_i))
        hand_set = Set.new(hand_str.split().map(&:to_i))
        count = (winning_set & hand_set).length
        if count > 0
            result += 2 ** (count - 1)
        end
    end
    result
  end
    
 def main()
    if ARGV.length < 1
      usage
    end
    
    filename = ARGV[0]
    contents = File.read(filename)
    result = process(contents)
    puts "result = #{result}"
  end
  
main()
