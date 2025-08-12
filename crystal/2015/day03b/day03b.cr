def usage
    puts "usage: #{PROGRAM_NAME} <input file>"
    exit 1
end

def process(filename)
    santa = {0, 0}
    robo_santa = {0, 0}
    santa_move = true
    positions = Set({Int32, Int32}).new
    positions.add(santa)
    File.each_line(filename) do |line|
        line.each_char do |ch|
            case ch
            when '^'
               new_pos = santa_move ? {santa[0], santa[1] + 1} : {robo_santa[0], robo_santa[1] + 1}
            when 'v'
               new_pos = santa_move ? {santa[0], santa[1] - 1} : {robo_santa[0], robo_santa[1] - 1}
            when '<'
               new_pos = santa_move ? {santa[0] - 1, santa[1]} : {robo_santa[0] - 1, robo_santa[1]}
            when '>'
               new_pos = santa_move ? {santa[0] + 1, santa[1]} : {robo_santa[0] + 1, robo_santa[1]}
            else
               next
            end
            positions.add(new_pos)
            if santa_move
                santa = new_pos
            else
                robo_santa = new_pos
            end
            santa_move = !santa_move
        end
    end
    positions.size
end

def main
    usage if ARGV.size < 1
    filename = ARGV[0]
    result = process(filename)
    puts "result = #{result}"
end

main

