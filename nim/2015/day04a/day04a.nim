import checksums/md5, os, strutils

proc processKey(key: string): int =
  inc result
  while true:
    let tryKey = key & $result
    let digest = getMD5(tryKey)
    if digest.startsWith("00000"):
      break
    inc result

when isMainModule:
  if paramCount() < 1:
    quit "usage: " & getAppFilename() & " <key>", 1

  let key = paramStr(1)
  echo "result = ", processKey(key)
