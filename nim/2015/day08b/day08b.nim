import os

proc processFile(filename: string): int =
  for line in lines(filename):
    let code_len = line.len
    var enc_len = 0
    for i in 0..<line.len:
      case line[i]
      of '\\', '"':
        enc_len += 2
      else:
        enc_len += 1
    result += 2 + (enc_len - code_len)
  return result

when isMainModule:
  if paramCount() < 1:
    quit "usage: " & getAppFilename() & " <input file>", 1

  let filename = paramStr(1)
  try:
    echo "result = ", processFile(filename)
  except OSError as e:
    quit "error while processing file `" & filename & "': " & e.msg, 1
