module Main ( main ) where

import Data.Char
import Data.List
import Data.Maybe
import System.Environment
import System.Exit
import Text.Regex.TDFA

usage :: IO ()
usage = do
  progname <- getProgName
  putStrLn $ "usage: " ++ progname ++ " <file>"
  exitFailure

process :: String -> Int
process contents =
    let extractMulExprs str = getAllTextMatches (str =~ "mul\\([0-9]+,[0-9]+\\)") :: [String]
        extractNumPair mulExpr = getAllTextMatches (mulExpr =~ "[0-9]+") :: [String]
        convertPair numPair = map read numPair :: [Int]
    in sum $ map product $ map convertPair $ map extractNumPair $ extractMulExprs contents

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      contents <- readFile filename
      let result = process contents
      putStrLn $ "result = " ++ show result
    _ -> usage
