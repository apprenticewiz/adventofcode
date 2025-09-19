import os

proc processFile(filename: string): int =
  for line in lines(filename):
    let code_len = line.len
    var i = 1
    var mem_len = 0
    while i < line.len - 1:
      case line[i]
      of '\\':
        case line[i + 1]
        of '\\', '\"':
          i += 2
        of 'x':
          i += 4
        else:
          i += 1
      else:
          i += 1
      mem_len += 1
    result += code_len - mem_len
  return result

when isMainModule:
  if paramCount() < 1:
    quit "usage: " & getAppFilename() & " <input file>", 1

  let filename = paramStr(1)
  try:
    echo "result = ", processFile(filename)
  except OSError as e:
    quit "error while processing file `" & filename & "': " & e.msg, 1
