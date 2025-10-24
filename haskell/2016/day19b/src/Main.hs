module Main ( main ) where

import System.Environment
import System.Exit
import System.IO

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

main :: IO ()
main = do
    args <- getArgs
    progname <- getProgName
    case args of
        [n] -> do
            let result = process $ read n
            putStrLn $ "result = " ++ show result
        _ -> usage progname
