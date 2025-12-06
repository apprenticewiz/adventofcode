module Main ( main ) where

import Control.DeepSeq
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import System.Clock
import System.Environment
import System.Exit
import System.IO

process :: String -> Int
process content =
    let ls   = lines content
        rows = length ls
        cols = length (head ls)
        grid = Map.fromList [ ((i, j), c) | i <- [0..rows - 1], j <- [0..cols - 1], let c = ls !! i !! j ]
    in go grid [] (cols - 1) 0
  where
    go :: Map (Int, Int) Char -> [Int] -> Int -> Int -> Int
    go _    _     (-1) acc = acc
    go grid stack col  acc =
        let cs = [ ch | ((_, j), ch) <- Map.assocs grid, j == col ]
        in if all ((==) ' ') cs
               then go grid [] (col - 1) acc
               else if last cs `elem` "+*"
                        then let op = last cs
                                 n = (read . trim) (init cs)
                                 stack' = (n : stack)
                                 v = case op of
                                         '+' -> sum stack'
                                         '*' -> product stack'
                                         _   -> error "expected '+' or '*'"
                             in go grid [] (col - 1) (acc + v)
                         else let n = (read . trim) cs
                              in go grid (n : stack) (col - 1) acc

    trim :: String -> String
    trim = takeWhile (/= ' ') . dropWhile (== ' ')

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
       else if ns < 10000000000
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
