module Main ( main ) where

import Data.List ( elemIndices, subsequences )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure )

usage :: IO ()
usage = do
  progname <- getProgName
  putStrLn $ "usage: " ++ progname ++ " <file>"
  exitFailure

scanReports :: [Int] -> Bool
scanReports levels =
    let diffs = zipWith (flip (-)) levels (tail levels)
    in all (\x -> x /= 0 && abs x <= 3) diffs && (all (\x -> signum x == 1) diffs || all (\x -> signum x == -1) diffs)

scanReportsWithRetries :: [Int] -> Bool
scanReportsWithRetries levels =
    scanReports levels || (let levelsCount = length levels
                               retries = filter (\x -> length x == levelsCount - 1) (subsequences levels)
                           in any scanReports retries)

process :: String -> Int
process contents = length $ elemIndices True $ map ((scanReportsWithRetries . map read) . words) (lines contents)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      contents <- readFile filename
      let result = process contents
      putStrLn $ "result = " ++ show result
    _ -> usage
