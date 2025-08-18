import os, sequtils, strutils

proc processFile(filename: string): int =
  for line in lines(filename):
    let dims = (line.split('x').map(parseInt))
    let (l, w, h) = (dims[0], dims[1], dims[2])
    let (perim1, perim2, perim3) = (2 * (l + w), 2 * (l + h), 2 * (w + h))
    let presentLen = min(perim1, min(perim2, perim3))
    let bowLen = l * w * h
    result += presentLen + bowLen

when isMainModule:
  if paramCount() < 1:
    quit "usage: " & getAppFilename() & " <input file>", 1

  let filename = paramStr(1)
  try:
    echo "result = ", processFile(filename)
  except OSError as e:
    quit "error while processing file `" & filename & "': " & e.msg, 1
