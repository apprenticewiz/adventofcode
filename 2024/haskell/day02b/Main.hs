module Main ( main ) where

import Data.List ( elemIndices, subsequences )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure )

data Direction = Unknown | Increasing | Decreasing
    deriving (Eq, Show)

usage :: IO ()
usage = do
  progname <- getProgName
  putStrLn $ "usage: " ++ progname ++ " <file>"
  exitFailure

scanReports :: [Int] -> Bool
scanReports levels =
    let doScan :: Direction -> [Int] -> Bool
        doScan _ [x] = True
        doScan dir (x:y:zs) =
            (not (x == y || abs (x - y) > 3) && (case dir of
                        Unknown ->
                            if y < x
                                then doScan Decreasing (y:zs)
                                else doScan Increasing (y:zs)
                        Increasing ->
                            (y >= x) && doScan dir (y:zs)
                        Decreasing ->
                            (y <= x) && doScan dir (y:zs)))
    in doScan Unknown levels

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
