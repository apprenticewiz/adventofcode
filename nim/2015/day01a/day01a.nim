import os

proc processFile(filename: string): int =
  for line in lines(filename):
    for ch in line:
      if ch == '(': inc result
      elif ch == ')': dec result

when isMainModule:
  if paramCount() < 1:
    quit "usage: " & getAppFilename() & " <input file>", 1

  let filename = paramStr(1)
  try:
    echo "result = ", processFile(filename)
  except OSError as e:
    quit "error while processing file `" & filename & "': " & e.msg, 1
