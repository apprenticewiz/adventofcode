module Main ( main ) where

import Data.List.Split
import Data.Maybe
import System.Environment
import System.Exit
import System.IO

type Range = (Int, Int)

process :: String -> Int
process content =
    case listToMaybe (lines content) of
        Nothing -> error "empty file"
        Just line ->
            let ranges = mapMaybe parsePair (splitOn "," line)
                invalidIds = foldl step [] ranges
            in sum invalidIds

  where
    parsePair :: String -> Maybe Range
    parsePair pair =
        case splitOn "-" pair of
            [a, b] -> Just (read a, read b)
            _      -> Nothing

    step :: [Int] -> Range -> [Int]
    step acc (lo, hi) =
        let nums = [lo..hi]
            invalid = filter isInvalid nums
        in acc ++ invalid

    isInvalid :: Int -> Bool
    isInvalid n =
        let len = digitLen n
        in even len && halfRepeat n len

    halfRepeat :: Int -> Int -> Bool
    halfRepeat n len =
        let m     = len `div` 2
            scale = 10 ^ m
            hi    = n `quot` scale
            lo    = n `mod`  scale
        in hi == lo

    digitLen :: Int -> Int
    digitLen x = go x 0
      where
        go 0 0 = 1
        go 0 c = c
        go y c = go (y `quot` 10) (c+1)

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

main :: IO ()
main = do
    args <- getArgs
    progname <- getProgName
    case args of
        [filename] -> do
            content <- readFile filename
            let result = process content
            putStrLn $ "result = " ++ show result
        _ -> usage progname
