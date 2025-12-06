module Main ( main ) where

import Control.DeepSeq
import Data.Array.Unboxed ( UArray )
import qualified Data.Array.Unboxed as A
import System.Clock
import System.Environment
import System.Exit
import System.IO

process :: String -> Int
process content =
    let ls   = lines content
        rows = length ls
        cols = if null ls then 0 else length (ls !! 0)
        grid = A.listArray ((0, 0), (rows - 1, cols - 1)) (concat ls) :: UArray (Int, Int) Char
    in go grid [] (cols - 1) 0
  where
    go :: UArray (Int, Int) Char -> [Int] -> Int -> Int -> Int
    go _    _     (-1) acc = acc
    go grid stack col  acc =
        let ((minRow, _), (maxRow, _)) = A.bounds grid
            cs = [ grid A.! (i, col) | i <- [minRow..maxRow] ]
        in if all (' ' ==) cs
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
