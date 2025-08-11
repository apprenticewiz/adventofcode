def usage
    puts "usage: #{PROGRAM_NAME} <input file>"
    exit 1
end

def process(filename)
    total_len = 0
    File.each_line(filename) do |line|
        l, w, h = line.split("x").map(&.to_i)
        perim1, perim2, perim3 = 2 * (l + w), 2 * (l + h), 2 * (w + h)
        present_len = [perim1, perim2, perim3].min
        bow_len = l * w * h
        total_len += present_len + bow_len
    end
    total_len
end

def main
    usage if ARGV.size < 1
    filename = ARGV[0]
    result = process(filename)
    puts "result = #{result}"
end

main

