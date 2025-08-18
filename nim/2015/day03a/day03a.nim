import os, sets

type
  Position2D = object
    x, y: int

proc `==`(a, b: Position2D): bool =
  a.x == b.x and a.y == b.y

proc hash(a: Position2D): int =
  (a.x shl 16) or a.y

proc processFile(filename: string): int =
  var positions = initHashSet[Position2D]()
  var santa = Position2D(x: 0, y: 0)
  positions.incl(santa)
  for line in lines(filename):
    for ch in line:
      if ch == '^': santa = Position2D(x: santa.x, y: santa.y + 1)
      elif ch == 'v': santa = Position2D(x: santa.x, y: santa.y - 1)
      elif ch == '<': santa = Position2D(x: santa.x - 1, y: santa.y)
      elif ch == '>': santa = Position2D(x: santa.x + 1, y: santa.y)
      positions.incl(santa)
  return positions.len()

when isMainModule:
  if paramCount() < 1:
    quit "usage: " & getAppFilename() & " <input file>", 1

  let filename = paramStr(1)
  try:
    echo "result = ", processFile(filename)
  except OSError as e:
    quit "error while processing file `" & filename & "': " & e.msg, 1
