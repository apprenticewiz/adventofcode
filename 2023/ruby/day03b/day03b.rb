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
  part_locs.each_key do |part_loc|
    adj_count = 0
    prod = 1
    number_locs.each do |num_loc, number|
      num_row = num_loc.row
      num_col_first = num_loc.col
      num_col_last = num_loc.col + number.length - 1
      found = false
      neighbors.each do |neighbor|
        adjacent_pos = Position.new(part_loc.row + neighbor.row, part_loc.col + neighbor.col)
        (num_col_first..num_col_last).each do |num_col|
          if (num_row == adjacent_pos.row) && (num_col == adjacent_pos.col)
            adj_count += 1
            prod *= number.to_i
            found = true
            break
          end
        end
        if found
          break
        end
      end
    end
    if adj_count == 2
      result += prod
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
