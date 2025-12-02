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
        let s = show n
            mid = length s `div` 2
            lens = [1..mid]
        in go s lens
      where
        go _ [] = False
        go s (m:ms) =
            let (firstPart, secondPart) = splitAt m s
                copies = length s `div` m - 1
                pattern = concat $ replicate copies firstPart
            in if pattern == secondPart
                then True
                else go s ms

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
