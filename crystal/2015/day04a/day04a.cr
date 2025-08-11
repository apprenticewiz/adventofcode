require "digest/md5"

def usage
    puts "usage: #{PROGRAM_NAME} <key>"
    exit 1
end

def process(key)
    n = 1
    while true
        try_key = key + n.to_s
        digest = Digest::MD5.hexdigest(try_key)
        if digest.starts_with?("00000")
            return n
        else
            n += 1
        end
    end
end

def main
    usage if ARGV.size < 1
    key = ARGV[0]
    result = process(key)
    puts "result = #{result}"
end

main

