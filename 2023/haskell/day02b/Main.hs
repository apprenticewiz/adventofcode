module Main ( main ) where

import Data.List
import System.Environment
import System.Exit
import System.IO

type Color = String
type Amount = Int
type CubeSet = (Amount, Amount, Amount)

usage :: IO a
usage = do
  progname <- getProgName
  putStrLn $ "usage: " ++ progname ++ " <file>"
  exitWith (ExitFailure 1)

process :: String -> Int
process contents = sum powers
  where
    powers = [redNeeded * greenNeeded * blueNeeded | line <- (lines contents)
                                                   , let revealsStr = head $ drop 1 (splitOn ": " line)
                                                   , let draws = map (splitOn ", ") (splitOn "; " revealsStr)
                                                   , let (redNeeded, greenNeeded, blueNeeded) = foldl (\(r, g, b) draw ->
                                                                                                          foldl (\(r', g', b') colorAmount ->
                                                                                                                  let amountStr = head (words colorAmount)
                                                                                                                      color = head $ drop 1 (words colorAmount)
                                                                                                                      amount = read amountStr :: Int
                                                                                                                  in case color of
                                                                                                                    "red"   -> (max r' amount, g', b')
                                                                                                                    "green" -> (r', max g' amount, b')
                                                                                                                    "blue"  -> (r', g', max b' amount)
                                                                                                                    _       -> error "unknown color"
                                                                                                          ) (r, g, b) draw
                                                                                                        ) (0, 0, 0) draws
      ]

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn _ [] = []
splitOn delimiter list =
  chunk : splitOn delimiter rest
  where
    (chunk, rest) = breakList delimiter list

breakList :: Eq a => [a] -> [a] -> ([a], [a])
breakList delimiter list =
  case stripPrefix delimiter list of
    Just suffix -> ([], suffix)
    Nothing ->
      case list of
        [] -> ([], [])
        x : xs -> let (chunk, rest) = breakList delimiter xs in (x : chunk, rest)

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
