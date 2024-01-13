#!/usr/bin/env ruby

def usage
    progname = $PROGRAM_NAME
    puts "usage: #{progname} <file>"
    exit(1)
  end
  
  def process(contents)
    result = 0
    digit_strs = {
        "0" => 0,
        "1" => 1,
        "2" => 2,
        "3" => 3,
        "4" => 4,
        "5" => 5,
        "6" => 6,
        "7" => 7,
        "8" => 8,
        "9" => 9,
        "zero" => 0,
        "one" => 1,
        "two" => 2,
        "three" => 3,
        "four" => 4,
        "five" => 5,
        "six" => 6,
        "seven" => 7,
        "eight" => 8,
        "nine" => 9,
      }

    contents.each_line do |line|
      min_index = nil
      max_index = nil
      left_digit = 0
      right_digit = 0
  
      digit_strs.each do |digit_str, digit|
        if line.include?(digit_str)
          left_index = line.index(digit_str)
          if min_index.nil? || left_index < min_index
            min_index = left_index
            left_digit = digit
          end
  
          right_index = line.rindex(digit_str)
          if max_index.nil? || right_index > max_index
            max_index = right_index
            right_digit = digit
          end
        end
      end
  
      result += (left_digit * 10) + right_digit
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
  