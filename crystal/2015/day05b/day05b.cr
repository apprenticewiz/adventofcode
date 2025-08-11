def usage
    puts "usage: #{PROGRAM_NAME} <input file>"
    exit 1
end

def prop1?(str)
    !!(str =~ /(..).*\1/)
end

def prop2?(str)
    !!(str =~ /(.).\1/)
end

def process(filename)
    counter = 0
    File.each_line(filename) do |line|
        counter += 1 if prop1?(line) && prop2?(line)
    end
    counter
end

def main
    usage if ARGV.size < 1
    filename = ARGV[0]
    result = process(filename)
    puts "result = #{result}"
end

main

