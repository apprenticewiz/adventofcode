#!/usr/bin/env ruby

Position = Struct.new(:row, :col)

def usage
  progname = $PROGRAM_NAME
  puts "usage: #{progname} <file>"
  exit(1)
end

def build_numbers(contents)
  number_locs = {}
  scanning_number = false
  number = ""
  current_pos = Position.new(-1, -1)
  contents.each_line.with_index do |line, row|
    line.strip().each_char.with_index do |ch, col|
      if scanning_number
        if ch.match?(/\d/)
          number = number + ch
        else
          number_locs[current_pos] = number
          number = ""
          scanning_number = false
        end
      else
        if ch.match?(/\d/)
          number = number + ch
          current_pos = Position.new(row, col)
          scanning_number = true
        end
      end
    end
    if scanning_number
      number_locs[current_pos] = number
      number = ""
      scanning_number = false
    end
  end
  number_locs
end

def build_parts(contents)
  part_locs = {}
  contents.each_line.with_index do |line, row|
    line.strip().each_char.with_index do |ch, col|
      if !ch.match?(/\d/) && ch != '.'
        part_locs[Position.new(row, col)] = ch
      end
    end
  end
  part_locs
end

def check_parts(number_locs, part_locs)
  result = 0
  neighbors = [
    Position.new(-1, -1), Position.new(-1, 0), Position.new(-1, 1),
    Position.new(0, -1), Position.new(0, 1),
    Position.new(1, -1), Position.new(1, 0), Position.new(1, 1)
  ]
  number_locs.each do |number_loc, number|
    number_row = number_loc.row
    number_col_first = number_loc.col
    number_col_last = number_loc.col + number.length - 1
    found = false
    (number_col_first..number_col_last).each do |number_col|
      neighbors.each do |neighbor|
        adjacent_pos = Position.new(number_row + neighbor.row, number_col + neighbor.col)
        part_locs.each_key do |part_loc|
          if (part_loc.row == adjacent_pos.row) && (part_loc.col == adjacent_pos.col)
            found = true
            break
          end
        end
        if found
          break
        end
      end
      if found
        break
      end
    end
    if found
      result += number.to_i
    end
  end
  result
end

def process(contents)
  number_locs = build_numbers(contents)
  part_locs = build_parts(contents)
  check_parts(number_locs, part_locs)
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
