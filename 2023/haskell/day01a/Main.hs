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

process :: String -> Int
process contents = sum $ map extractDigits $ lines contents
  where
    digits = "0123456789"
    extractDigits :: String -> Int
    extractDigits line = ((digitToInt leftDigit) * 10) + (digitToInt rightDigit)
      where
        occurrences = filter (`elem` digits) line
        leftDigit = head occurrences
        rightDigit = last occurrences

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      contents <- readFile filename
      let result = process contents
      putStrLn $ "result = " ++ show result
    _ -> usage
