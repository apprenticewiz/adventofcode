module Main ( main ) where

import Data.List ( elemIndices )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure )

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
                secondNumber = read (numbersList !! 1) :: Int
            in (firstList ++ [firstNumber], secondList ++ [secondNumber])
        )
        ([], [])
        (lines contents)

computeScores :: [Int] -> [Int] -> [Int]
computeScores firstList secondList = map (\firstNumber -> firstNumber * length (elemIndices firstNumber secondList)) firstList

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
