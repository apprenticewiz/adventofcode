import os, re, strutils

proc perform(grid: var array[1000*1000, bool], action: string, r1: int, c1: int, r2: int, c2: int) =
  for row in r1..r2:
    for col in c1..c2:
      if action == "turn on":
        grid[row*1000 + col] = true
      elif action == "turn off":
        grid[row*1000 + col] = false
      elif action == "toggle":
        grid[row*1000 + col] = not grid[row*1000 + col]

proc count(grid: array[1000*1000, bool]): int =
  var total = 0
  for row in 0..999:
    for col in 0..999:
      if grid[row*1000 + col]:
        total += 1
  total

proc processFile(filename: string): int =
  var grid: array[1000*1000, bool]
  var caps: array[5, string];
  for line in lines(filename):
    if find(line, re"(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)", caps) != -1:
      let action = caps[0]
      let r1 = parseInt(caps[1])
      let c1 = parseInt(caps[2])
      let r2 = parseInt(caps[3])
      let c2 = parseInt(caps[4])
      perform(grid, action, r1, c1, r2, c2)
  count(grid)

when isMainModule:
  if paramCount() < 1:
    quit "usage: " & getAppFilename() & " <input file>", 1

  let filename = paramStr(1)
  try:
    echo "result = ", processFile(filename)
  except OSError as e:
    quit "error while processing file `" & filename & "': " & e.msg, 1
