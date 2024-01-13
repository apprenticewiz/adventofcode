#!/usr/bin/env ruby

TOTAL_RED = 12
TOTAL_GREEN = 13
TOTAL_BLUE = 14

def usage
    progname = $PROGRAM_NAME
    puts "usage: #{progname} <file>"
    exit(1)
  end
  
  def process(contents)
    result = 0
 
    contents.each_line do |line|
      if (match_data = line.match(/^(?<game_str>.*): (?<reveals_str>.*)$/))
        game_str, reveals_str = match_data[:game_str], match_data[:reveals_str]
  
        if (match_data = game_str.match(/^\S+ (?<game_num_str>\d+)$/))
          game_num_str = match_data[:game_num_str]
          game_num = game_num_str.to_i
          valid = true
  
          reveals_str.split('; ').each do |subset_str|
            subset_str.split(', ').each do |cubes_str|
              if (match_data = cubes_str.match(/^(?<amount_str>\d+) (?<color>\w+)$/))
                amount_str, color = match_data[:amount_str], match_data[:color]
                amount = amount_str.to_i
  
                case color
                when 'red'
                  valid = false if amount > TOTAL_RED
                when 'green'
                  valid = false if amount > TOTAL_GREEN
                when 'blue'
                  valid = false if amount > TOTAL_BLUE
                else
                  raise 'unknown color'
                end
              end
            end
          end
  
          result += game_num if valid
        end
      end  
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
  