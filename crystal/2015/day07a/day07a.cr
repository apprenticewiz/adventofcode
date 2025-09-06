class Operation
    enum Operator
        Assign
        Not
        And
        Or
        LeftShift
        RightShift
    end

    getter operator : Operator
    getter operands : Tuple(String, Nil | String | Int32)

    def initialize(@operator : Operator, @operands : Tuple(String, Nil | String | Int32))
    end
end

def usage
    puts "usage: #{PROGRAM_NAME} <input file>"
    exit 1
end

def process(filename)
    operations = Hash(String, Operation).new
    cache = Hash(String, Int32).new
    re1 = /^(\d+|\w+) -> (\w+)$/
    re2 = /NOT (\d+|\w+) -> (\w+)/
    re3 = /(\d+|\w+) (AND|OR) (\d+|\w+) -> (\w+)/
    re4 = /(\d+|\w+) (LSHIFT|RSHIFT) (\d+) -> (\w+)/
    File.each_line(filename) do |line|
        if matches = re1.match(line)
            src = matches[1]
            dest = matches[2]
            operations[dest] = Operation.new(Operation::Operator::Assign, {src, nil})
        elsif matches = re2.match(line)
            src = matches[1]
            dest = matches[2]
            operations[dest] = Operation.new(Operation::Operator::Not, {src, nil})
        elsif matches = re3.match(line)
            src1 = matches[1]
            op = (matches[2] == "AND") ? Operation::Operator::And : Operation::Operator::Or
            src2 = matches[3]
            dest = matches[4]
            operations[dest] = Operation.new(op, {src1, src2})
        elsif matches = re4.match(line)
            src = matches[1]
            op = (matches[2] == "LSHIFT") ? Operation::Operator::LeftShift : Operation::Operator::RightShift
            amt = matches[3].to_i
            dest = matches[4]
            operations[dest] = Operation.new(op, {src, amt})
        end
    end
    evaluate(operations, cache, "a")
end

def evaluate(ops, cache, expr)
    if expr.to_i?
        expr.to_i
    elsif cache.has_key?(expr)
        cache[expr]
    else
        op = ops[expr]
        if op.operator == Operation::Operator::Assign
             src = op.operands[0]
             a = evaluate(ops, cache, src)
             r = a
        elsif op.operator == Operation::Operator::Not
            src = op.operands[0]
            a = evaluate(ops, cache, src)
            r = ~a
        elsif op.operator == Operation::Operator::And
            src1 = op.operands[0]
            src2 = op.operands[1].as(String)
            a = evaluate(ops, cache, src1)
            b = evaluate(ops, cache, src2)
            r = a & b
        elsif op.operator == Operation::Operator::Or
            src1 = op.operands[0]
            src2 = op.operands[1].as(String)
            a = evaluate(ops, cache, src1)
            b = evaluate(ops, cache, src2)
            r = a | b
        elsif op.operator == Operation::Operator::LeftShift
            src = op.operands[0]
            amt = op.operands[1].as(Int32)
            a = evaluate(ops, cache, src)
            r = a << amt
        elsif op.operator == Operation::Operator::RightShift
            src = op.operands[0]
            amt = op.operands[1].as(Int32)
            a = evaluate(ops, cache, src)
            r = a >> amt
        else
            r = 0
        end
        masked = r & 0xffff
        cache[expr] = masked
        masked
    end
end

def main
    usage if ARGV.size < 1
    filename = ARGV[0]
    result = process(filename)
    puts "result = #{result}"
end

main

