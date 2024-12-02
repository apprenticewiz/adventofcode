module Main ( main ) where

import Data.Char
import Data.List
import Data.Maybe
import System.Environment
import System.Exit

usage :: IO ()
usage = do
  progname <- getProgName
  putStrLn $ "usage: " ++ progname ++ " <file>"
  exitFailure

buildLists :: String -> ([Int], [Int])
buildLists contents =
    foldl
        (\(firstList, secondList) line ->
            let numbersList = words line
                firstNumber = read $ head numbersList :: Int
                secondNumber = read $ head $ drop 1 numbersList :: Int
            in (firstList ++ [firstNumber], secondList ++ [secondNumber])
        )
        ([], [])
        (lines contents)

computeScores :: [Int] -> [Int] -> [Int]
computeScores firstList secondList =
    foldl
        (\scores firstNumber ->
            let score = firstNumber * (length $ elemIndices firstNumber secondList)
            in scores ++ [score]
        )
        []
        firstList

process :: String -> Int
process contents =
    let (firstList, secondList) = buildLists contents
        scores = computeScores firstList secondList
    in sum scores

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      contents <- readFile filename
      let result = process contents
      putStrLn $ "result = " ++ show result
    _ -> usage
