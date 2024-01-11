module Main ( main ) where

import Data.Char
import qualified Data.Map.Strict as Map
import Data.List
import Data.Maybe
import System.Environment
import System.Exit

usage :: IO ()
usage = do
  progname <- getProgName
  putStrLn $ "usage: " ++ progname ++ " <file>"
  exitFailure

process :: String -> Int
process contents = 
  let contentLines = lines contents
  in sum $ map processLine contentLines

processLine :: String -> Int
processLine line =
  let digitsMap = Map.fromList [
        ("0", 0), ("1", 1), ("2", 2), ("3", 3), ("4", 4), ("5", 5), ("6", 6),
        ("7", 7), ("8", 8), ("9", 9), ("zero", 0), ("one", 1), ("two", 2),
        ("three", 3), ("four", 4), ("five", 5), ("six", 6), ("seven", 7),
        ("eight", 8), ("nine", 9) ]
      findString needle haystack = findIndices (isPrefixOf needle) (tails haystack)
      (minPair, maxPair) = foldl (\pair digit ->
          let occurrences = findString digit line
          in case pair of
            (Nothing, Nothing) ->
              if (null occurrences)
                then (Nothing, Nothing)
                else (Just (digit, (head occurrences)), Just (digit, (last occurrences)))
            (Just (minDigit, minIndex), Just (maxDigit, maxIndex)) ->
              if (null occurrences)
                then (Just (minDigit, minIndex), Just (maxDigit, maxIndex))
                else (Just (if (head occurrences) < minIndex
                              then (digit, (head occurrences))
                              else (minDigit, minIndex)),
                      Just (if (last occurrences) > maxIndex
                              then (digit, (last occurrences))
                              else (maxDigit, maxIndex)))
            _ -> undefined
        ) (Nothing, Nothing) (Map.keys digitsMap)
      leftDigit = fromJust $ Map.lookup (fst $ fromJust minPair) digitsMap
      rightDigit = fromJust $ Map.lookup (fst $ fromJust maxPair) digitsMap
  in (leftDigit * 10) + rightDigit

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      contents <- readFile filename
      let result = process contents
      putStrLn $ "result = " ++ show result
    _ -> usage
