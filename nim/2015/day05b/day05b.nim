import os, re

proc prop1(s: string): bool =
  return contains(s, re"(..).*\1")

proc prop2(s: string): bool =
  return contains(s, re"(.).\1")

proc processFile(filename: string): int =
  for line in lines(filename):
    if prop1(line) and prop2(line):
      inc result

when isMainModule:
  if paramCount() < 1:
    quit "usage: " & getAppFilename() & " <input file>", 1

  let filename = paramStr(1)
  try:
    echo "result = ", processFile(filename)
  except OSError as e:
    quit "error while processing file `" & filename & "': " & e.msg, 1
