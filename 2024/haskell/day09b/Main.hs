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

findFirstGapOfSizeBefore :: Int -> Int -> DiskImage -> Maybe (Int, Int)
findFirstGapOfSizeBefore gapSize loc diskImage = findFirstGapOfSizeBefore' gapSize loc (zip diskImage (tail diskImage))
    where findFirstGapOfSizeBefore' gapSize loc [] = Nothing
          findFirstGapOfSizeBefore' gapSize loc (pair:rest) =
              let (file1, file2) = pair
                  (file1Start, file1Size, _) = file1
                  (file2Start, _, _) = file2
                  file1End = file1Start + file1Size - 1
                  currentGapStart = file1End + 1
                  currentGapSize = file2Start - file1End - 1
              in if currentGapStart >= loc
                    then Nothing
                    else if currentGapSize >= gapSize
                        then Just (currentGapStart, currentGapSize)
                        else findFirstGapOfSizeBefore' gapSize loc rest

compact :: DiskImage -> DiskImage
compact diskImage =
    let (_, _, lastId) = last diskImage
    in compact' diskImage lastId
        where compact' diskImage 0 = diskImage
              compact' diskImage currentId =
                  let fileEntry = head $ filter (\(_, _, fileId) -> fileId == currentId) diskImage
                      (fileStart, fileSize, _) = fileEntry
                      firstGap = findFirstGapOfSizeBefore fileSize fileStart diskImage
                  in case firstGap of
                      Nothing ->
                          if currentId == 0
                              then diskImage
                              else compact' diskImage (currentId - 1)
                      Just (gapStart, gapSize) ->
                          if fileSize > gapSize
                              then compact' diskImage (currentId - 1)
                              else let tempImage = filter (\(_, _, fileId) -> fileId /= currentId) diskImage
                                       newEntry = (gapStart, fileSize, currentId)
                                   in compact' (sort (tempImage ++ [newEntry])) (currentId - 1)

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
