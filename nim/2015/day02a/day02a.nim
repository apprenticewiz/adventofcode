import os, sequtils, strutils

proc processFile(filename: string): int =
  for line in lines(filename):
    let dims = (line.split('x').map(parseInt))
    let (l, w, h) = (dims[0], dims[1], dims[2])
    let (area1, area2, area3) = (l * w, l * h, w * h)
    let surfaceArea = 2 * area1 + 2 * area2 + 2 * area3
    let minArea = min(area1, min(area2, area3))
    result += surfaceArea + minArea

when isMainModule:
  if paramCount() < 1:
    quit "usage: " & getAppFilename() & " <input file>", 1

  let filename = paramStr(1)
  try:
    echo "result = ", processFile(filename)
  except OSError as e:
    quit "error while processing file `" & filename & "': " & e.msg, 1
