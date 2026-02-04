module Main ( main ) where

import System.Environment
import System.Exit
import System.IO
import Control.DeepSeq
import System.Clock

numRows :: Int
numRows = 40

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <n>"
    exitFailure

josephus :: Int -> Int
josephus n =
    let p = 3 ^ floor (logBase 3 (fromIntegral n))
    in if n <= 2 * p
         then n - p
         else 2 * n - 3 * p

process :: Int -> Int
process = josephus


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
        [n] -> do
            start <- getTime Monotonic
            let result = process $ read n
            result `deepseq` return ()
            end <- getTime Monotonic
            let elapsed = diffTimeSpec start end
            putStrLn $ "result = " ++ show result
            putStrLn $ "elapsed time: " ++ showTime elapsed
        _ -> usage progname
