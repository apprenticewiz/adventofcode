#!/usr/bin/env ruby

def usage
    progname = $PROGRAM_NAME
    puts "usage: #{progname} <file>"
    exit(1)
  end
  
  def process(contents)
    result = 0
    digits = %w[0 1 2 3 4 5 6 7 8 9]
  
    contents.each_line do |line|
      min_index = nil
      max_index = nil
      left_digit = '0'
      right_digit = '0'
  
      digits.each do |digit|
        if line.include?(digit)
          left_index = line.index(digit)
          if min_index.nil? || left_index < min_index
            min_index = left_index
            left_digit = digit.chars.first
          end
  
          right_index = line.rindex(digit)
          if max_index.nil? || right_index > max_index
            max_index = right_index
            right_digit = digit.chars.first
          end
        end
      end
  
      result += ((left_digit.ord - '0'.ord) * 10) + (right_digit.ord - '0'.ord)
    end
  
    result
  end
  
  if ARGV.length < 1
    usage
  end
  
  filename = ARGV[0]
  contents = File.read(filename)
  result = process(contents)
  puts "result = #{result}"
  