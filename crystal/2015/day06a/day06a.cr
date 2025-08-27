def usage
    puts "usage: #{PROGRAM_NAME} <input file>"
    exit 1
end

def perform(grid, action, r1, c1, r2, c2)
    (r1..r2).each do |row|
        (c1..c2).each do |col|
            case action
                when "turn on"
                    grid[row*1000 + col] = true
                when "turn off"
                    grid[row*1000 + col] = false
                when "toggle"
                    grid[row*1000 + col] = !grid[row*1000 + col]
            end
        end
    end
end

def count(grid)
    total = 0
    (0..999).each do |row|
        (0..999).each do |col|
            if grid[row*1000 + col]
                total += 1
            end
        end
    end
    total
end

def process(filename)
    grid = Array(Bool).new(1000*1000, false)
    re = /(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)/
    File.each_line(filename) do |line|
        if matches = re.match(line)
            action = matches[1]
            r1 = matches[2].to_i
            c1 = matches[3].to_i
            r2 = matches[4].to_i
            c2 = matches[5].to_i
            perform(grid, action, r1, c1, r2, c2)
        end
    end
    count(grid)
end

def main
    usage if ARGV.size < 1
    filename = ARGV[0]
    result = process(filename)
    puts "result = #{result}"
end

main

