module Main ( main ) where

import Data.Char ( digitToInt )
import Data.List ( sort )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure )

type DiskImage = [(Int, Int, Int)]

usage :: IO ()
usage = do
  progname <- getProgName
  putStrLn $ "usage: " ++ progname ++ " <file>"
  exitFailure

scanFileMap :: String -> DiskImage
scanFileMap contents = scanFileMap' contents 0 0 []
    where scanFileMap' [] _ _ diskImage = diskImage
          scanFileMap' (sizeCh:skipCh:rest) fileId start diskImage =
              let size = digitToInt sizeCh
                  skip = digitToInt skipCh
              in scanFileMap' rest (fileId + 1) (start + size + skip) (diskImage ++ [(start, size, fileId)])

findFirstGap :: DiskImage -> Maybe (Int, Int)
findFirstGap diskImage = findFirstGap' (zip diskImage (tail diskImage))
    where findFirstGap' [] = Nothing
          findFirstGap' (pair:rest) =
              let (file1, file2) = pair
                  (file1Start, file1Size, _) = file1
                  (file2Start, _, _) = file2
                  file1End = file1Start + file1Size - 1
              in if file2Start - file1End - 1 /= 0
                  then Just (file1End + 1, file2Start - file1End - 1)
                  else findFirstGap' rest

compact :: DiskImage -> DiskImage
compact diskImage =
    let firstGap = findFirstGap diskImage
    in case firstGap of
        Nothing -> diskImage
        Just (gapStart, gapSize) ->
            let (lastStart, lastSize, lastFileId) = last diskImage
            in if lastSize <= gapSize
                then let newEntry = (gapStart, lastSize, lastFileId)
                         newImage = sort (init diskImage ++ [newEntry])
                     in compact newImage
                else let newEntry1 = (gapStart, gapSize, lastFileId)
                         newEntry2 = (lastStart, lastSize - gapSize, lastFileId)
                         newImage = sort (init diskImage ++ [newEntry1, newEntry2])
                     in compact newImage

calcChecksum :: DiskImage -> Int
calcChecksum =
    foldl
        (\checksum entry ->
            let (start, size, fileId) = entry
                entryChecksums = map (* fileId) [start..(start + size - 1)]
            in checksum + sum entryChecksums
        )
        0

process :: String -> Int
process contents =
    let diskImage = sort (scanFileMap contents)
        compactImage = compact diskImage
    in calcChecksum compactImage

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      contents <- readFile filename
      let result = process contents
      putStrLn $ "result = " ++ show result
    _ -> usage
