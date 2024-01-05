module Main ( main ) where

import Data.List
import System.Environment
import System.Exit
import System.IO

totalRed :: Int
totalRed = 12

totalGreen :: Int
totalGreen = 13

totalBlue :: Int
totalBlue = 14

usage :: IO a
usage = do
  progname <- getProgName
  putStrLn $ "usage: " ++ progname ++ " <file>"
  exitWith (ExitFailure 1)

process :: String -> Int
process contents = sum validGames
  where
    validGames = [gameNum | line <- lines contents
                           , let gameStr =  head (splitOn ": " line)
                           , let revealsStr = head $ drop 1 (splitOn ": " line)
                           , let gameNumStr = head $ drop 1 (words gameStr)
                           , let gameNum = read gameNumStr :: Int
                           , let valid = all (\subsetStr ->
                                              all (\cubesStr ->
                                                     let color = head $ drop 1 (words cubesStr)
                                                         amountStr = head (words cubesStr)
                                                         amount = read amountStr :: Int
                                                     in case color of
                                                          "red"   -> amount <= totalRed
                                                          "green" -> amount <= totalGreen
                                                          "blue"  -> amount <= totalBlue
                                                          _       -> error ("unknown color: '" ++ color ++ "'")
                                                  ) (splitOn ", " subsetStr)
                                          ) (splitOn "; " revealsStr)
                           , valid
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
