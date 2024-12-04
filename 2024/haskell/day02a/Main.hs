module Main ( main ) where

import Data.List ( elemIndices )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure )

usage :: IO ()
usage = do
  progname <- getProgName
  putStrLn $ "usage: " ++ progname ++ " <file>"
  System.Exit.exitFailure

scanReports :: [Int] -> Bool
scanReports levels =
    let calcDiffs [x] = []
        calcDiffs (x:y:zs) = (y - x) : calcDiffs (y:zs)
        diffs = calcDiffs levels
    in all (\x -> x /= 0 && abs x <= 3) diffs && (all (\x -> signum x == 1) diffs || all (\x -> signum x == -1) diffs)

process :: String -> Int
process contents = length $ elemIndices True $ map ((scanReports . map read) . words) (lines contents)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      contents <- readFile filename
      let result = process contents
      putStrLn $ "result = " ++ show result
    _ -> usage
