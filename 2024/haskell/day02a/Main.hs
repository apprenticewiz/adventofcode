module Main ( main ) where

import Data.Char
import Data.List
import Data.Maybe
import System.Environment
import System.Exit

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
                            y >= x && doScan dir (y:zs)
                        Decreasing ->
                            (y <= x) && doScan dir (y:zs)))
    in doScan Unknown levels

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
