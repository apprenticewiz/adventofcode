import options, os, re, strutils, tables

type
  Operator = enum
    Assign, Not, And, Or, LeftShift, RightShift

type
  Operation = object
    operator: Operator
    source1: string
    source2: Option[string] = none(string)
    amount: Option[int] = none(int)

proc eval(ops: TableRef[string, Operation], cache: TableRef[string, int], expr: string): int =
  if match(expr, re"\d+"):
    parseInt(expr)
  elif cache.hasKey(expr):
    cache[expr]
  else:
    let op = ops[expr]
    var r: int;
    case op.operator:
    of Assign:
      let a = eval(ops, cache, op.source1)
      r = a
    of Not:
      let a = eval(ops, cache, op.source1)
      r = not a
    of And:
      let a = eval(ops, cache, op.source1)
      let b = eval(ops, cache, op.source2.get())
      r = a and b
    of Or:
      let a = eval(ops, cache, op.source1)
      let b = eval(ops, cache, op.source2.get())
      r = a or b
    of LeftShift:
      let a = eval(ops, cache, op.source1)
      r = a shl op.amount.get()
    of RightShift:
      let a = eval(ops, cache, op.source1)
      r = a shr op.amount.get()
    let masked = r and 0xffff;
    cache[expr] = masked
    masked

proc processFile(filename: string): int =
  var operations = newTable[string, Operation]()
  var cache = newTable[string, int]()
  var caps: array[4, string];
  for line in lines(filename):
    if find(line, re"^(\d+|\w+) -> (\w+)$", caps) != -1:
      let src1 = caps[0]
      let dest = caps[1]
      operations[dest] = Operation(operator: Assign, source1: src1)
    elif find(line, re"NOT (\d+|\w+) -> (\w+)", caps) != -1:
      let src1 = caps[0]
      let dest = caps[1]
      operations[dest] = Operation(operator: Not, source1: src1)
    elif find(line, re"(\d+|\w+) (AND|OR) (\d+|\w+) -> (\w+)", caps) != -1:
      let src1 = caps[0]
      let op = caps[1]
      let src2 = caps[2]
      let dest = caps[3]
      operations[dest] = Operation(operator: (if op == "AND": And else: Or), source1: src1, source2: some(src2))
    elif find(line, re"(\d+|\w+) (LSHIFT|RSHIFT) (\d+) -> (\w+)", caps) != -1:
      let src1 = caps[0]
      let op = caps[1]
      let amt = parseInt(caps[2])
      let dest = caps[3]
      operations[dest] = Operation(operator: (if op == "LSHIFT": LeftShift else: RightShift), source1: src1, amount: some(amt))
  eval(operations, cache, "a")

when isMainModule:
  if paramCount() < 1:
    quit "usage: " & getAppFilename() & " <input file>", 1

  let filename = paramStr(1)
  try:
    echo "result = ", processFile(filename)
  except OSError as e:
    quit "error while processing file `" & filename & "': " & e.msg, 1
