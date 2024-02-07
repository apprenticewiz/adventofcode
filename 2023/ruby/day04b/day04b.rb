#!/usr/bin/env ruby

require 'set'

def usage
    progname = $PROGRAM_NAME
    puts "usage: #{progname} <file>"
    exit(1)
  end
  
def process(contents)
    result = 0
    instances = {}
    contents.each_line do |line|
        card_part = line.split(%r{:\s+})[0]
        card_num = card_part.split(%r{\s+})[1].to_i
        rest = line.split(%r{:\s+})[1]
        winning_str = rest.split(%r{\s+\|\s+})[0]
        hand_str = rest.split(%r{\s+\|\s+})[1]
        winning_set = Set.new(winning_str.split().map(&:to_i))
        hand_set = Set.new(hand_str.split().map(&:to_i))
        count = (winning_set & hand_set).length
        ((card_num + 1)..(card_num + count)).each do |i|
          copies = ((instances[i] != nil) ? instances[i] : 0) + 1 + \
            (instances[card_num] != nil ? instances[card_num] : 0)
          instances[i] = copies
        end
        result += 1
    end
    instances.each_value do |n|
      result += n
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
