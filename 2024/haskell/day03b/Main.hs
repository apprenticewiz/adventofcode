module Main ( main ) where

import Data.Char
import Data.List
import Data.Maybe
import System.Environment
import System.Exit
import Text.Regex.TDFA

import Debug.Trace

usage :: IO ()
usage = do
  progname <- getProgName
  putStrLn $ "usage: " ++ progname ++ " <file>"
  exitFailure

process :: String -> Int
process contents =
    let extractIsns str = getAllTextMatches (str =~ "mul\\([0-9]+,[0-9]+\\)|don't|do") :: [String]
        extractNumPair mulExpr = getAllTextMatches (mulExpr =~ "[0-9]+") :: [String]
        convertPair numPair = map read numPair :: [Int]
    in fst $ foldl
                (\(acc, enabled) isn ->
                  case isn of
                    "do" -> (acc, True)
                    "don't" -> (acc, False)
                    _ -> if enabled
                            then (acc + (product $ convertPair $ extractNumPair isn), enabled)
                            else (acc, enabled)
                )
                (0, True)
                (extractIsns contents)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      contents <- readFile filename
      let result = process contents
      putStrLn $ "result = " ++ show result
    _ -> usage
