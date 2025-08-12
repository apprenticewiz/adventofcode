def usage
    puts "usage: #{PROGRAM_NAME} <input file>"
    exit 1
end

def process(filename)
    santa = {0, 0}
    positions = Set({Int32, Int32}).new
    positions.add(santa)
    File.each_line(filename) do |line|
        line.each_char do |ch|
            case ch
            when '^'
                santa = {santa[0], santa[1] + 1}
            when 'v'
                santa = {santa[0], santa[1] - 1}
            when '<'
                santa = {santa[0] - 1, santa[1]}
            when '>'
                santa = {santa[0] + 1, santa[1]}
            end
            positions.add(santa)
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

