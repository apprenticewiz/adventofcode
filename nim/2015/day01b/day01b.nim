import os

proc processFile(filename: string): int =
  var floor = 0
  var pos = 0
  for line in lines(filename):
    for ch in line:
      inc pos
      if ch == '(': inc floor
      elif ch == ')': dec floor
      if floor < 0: return pos
  return 0

when isMainModule:
  if paramCount() < 1:
    quit "usage: " & getAppFilename() & " <input file>", 1

  let filename = paramStr(1)
  try:
    echo "result = ", processFile(filename)
  except OSError as e:
    quit "error while processing file `" & filename & "': " & e.msg, 1
