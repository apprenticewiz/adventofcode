module Main ( main ) where

import Data.Char
import qualified Data.Set as Set
import Data.Maybe
import System.Environment
import System.Exit
import System.IO

splitWhen     :: (Char -> Bool) -> String -> [String]
splitWhen pred s =  case dropWhile pred s of
                      "" -> []
                      s' -> w : splitWhen pred s''
                            where (w, s'') = break pred s'

process :: String -> Int
process contents =
  foldl
    (\result line ->
      let rest = head $ drop 1 $ splitWhen (== ':') line
          winningStr = head $ splitWhen (== '|') rest
          winningSet = Set.fromList $ (map read (words winningStr) :: [Int])
          handStr = head $ drop 1 $ splitWhen (== '|') rest
          handSet = Set.fromList $ (map read (words handStr) :: [Int])
          commonCount = length (Set.intersection winningSet handSet)
      in result + (if (commonCount > 0) then (2 ^ (commonCount - 1)) else 0)
    )
    0
    (lines contents)

usage :: IO a
usage = do
  progname <- getProgName
  putStrLn $ "usage: " ++ progname ++ " <file>"
  exitWith (ExitFailure 1)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> usage
    [filename] -> do
      contents <- readFile filename
      let result = process contents
      putStrLn $ "result = " ++ show result
    _ -> usage
