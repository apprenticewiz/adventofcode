#!/usr/bin/env ruby

class Operation
  attr_reader :op, :operands

  def initialize(op, *operands)
    @op = op       # :assign, :not, :and, :or, :lshift, :rshift
    @operands = operands
  end
end

def usage
    progname = File.basename($0)
    puts "usage: #{progname} <input file>"
    exit 1
end

def process(filename)
    operations = {}
    cache = {}
    File.foreach(filename) do |line|
        case line
        when /^(\d+|\w+) -> (\w+)$/
            src, dest = $1, $2
            operations[dest] = Operation.new(:assign, src)
        when /NOT (\d+|\w+) -> (\w+)/
            src, dest = $1, $2
            operations[dest] = Operation.new(:not, src)
        when /(\d+|\w+) (AND|OR) (\d+|\w+) -> (\w+)/
            src1, op_str, src2, dest = $1, $2, $3, $4
            op = (op_str == "AND" ? :and : :or)
            operations[dest] = Operation.new(op, src1, src2)
        when /(\d+|\w+) (LSHIFT|RSHIFT) (\d+) -> (\w+)/
            src, op_str, amt, dest = $1, $2, $3.to_i, $4
            op = (op_str == "LSHIFT" ? :lshift : :rshift)
            operations[dest] = Operation.new(op, src, amt)
        end
    end
    a = evaluate(operations, cache, 'a')
    operations["b"] = Operation.new(:assign, a.to_s)
    cache = {}
    evaluate(operations, cache, 'a')
end

def evaluate(ops, cache, expr)
  return expr.to_i if expr =~ /^\d+$/
  return cache[expr] if cache.key?(expr)
  op = ops.fetch(expr)
  r = case op.op
      when :assign
        evaluate(ops, cache, op.operands[0])
      when :not
        ~evaluate(ops, cache, op.operands[0])
      when :and
        a = evaluate(ops, cache, op.operands[0])
        b = evaluate(ops, cache, op.operands[1])
        a & b
      when :or
        a = evaluate(ops, cache, op.operands[0])
        b = evaluate(ops, cache, op.operands[1])
        a | b
      when :lshift
        a = evaluate(ops, cache, op.operands[0])
        a << op.operands[1]
      when :rshift
        a = evaluate(ops, cache, op.operands[0])
        a >> op.operands[1]
      else
        0
      end
  cache[expr] = r & 0xffff
end

def main
    usage if ARGV.length < 1
    filename = ARGV[0]
    result = process(filename)
    puts "result = #{result}"
end

main if __FILE__ == $0
