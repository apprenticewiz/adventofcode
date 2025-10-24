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
    let l = 2 ^ floor (logBase 2 (fromIntegral n))
    in 2 * (n - l) + 1

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
