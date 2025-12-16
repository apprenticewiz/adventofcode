module Main ( main ) where

import Control.DeepSeq
import Data.List.Split
import Data.Maybe
import System.Clock
import System.Environment
import System.Exit
import System.IO

type Range = (Int, Int)

process :: String -> Int
process =
    sum . concatMap (uncurry invalidsInRange) . concatMap parseLine . take 1 . lines
  where
    parseLine :: String -> [Range]
    parseLine = mapMaybe parsePair . splitOn ","

    parsePair :: String -> Maybe Range
    parsePair pair =
        case splitOn "-" pair of
            [a, b] -> Just (read a, read b)
            _      -> Nothing

    invalidsInRange :: Int -> Int -> [Int]
    invalidsInRange lo hi = filter isInvalid [lo..hi]

    isInvalid :: Int -> Bool
    isInvalid n =
        let len = digitLen n
            mid = len `div` 2
        in any (hasPeriod n len) [1..mid]

    hasPeriod :: Int -> Int -> Int -> Bool
    hasPeriod n len m
      | len `mod` m /= 0 = False
      | otherwise =
        let k = len `div` m
            p = prefix n m
        in assemble p k m == n

    digitLen :: Int -> Int
    digitLen x = go x 0
      where
        go 0 0 = 1
        go 0 c = c
        go y c = go (y `quot` 10) (c + 1)

    prefix :: Int -> Int -> Int
    prefix n m = n `quot` (10 ^ (digitLen n - m))

    assemble :: Int -> Int -> Int -> Int
    assemble p k m = go k 0
      where
        pow = 10 ^ m
        go 0 acc = acc
        go r acc = go (r - 1) (acc * pow + p)

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

showTime :: TimeSpec -> String
showTime elapsed =
    let ns = fromIntegral (toNanoSecs elapsed) :: Double
    in if ns < 1000
       then show ns ++ " ns"
       else if ns < 1000000
       then show (ns / 1000.0) ++ " μs"
       else if ns < 1000000000
            then show (ns / 1000000.0) ++ " ms"
            else show (ns / 1000000000.0) ++ " s"

main :: IO ()
main = do
    args <- getArgs
    progname <- getProgName
    case args of
        [filename] -> do
            start <- getTime Monotonic
            content <- readFile filename
            let result = process content
            result `deepseq` return ()
            end <- getTime Monotonic
            let elapsed = diffTimeSpec start end
            putStrLn $ "result = " ++ show result
            putStrLn $ "elapsed time: " ++ showTime elapsed
        _ -> usage progname
