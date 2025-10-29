module Main ( main ) where

import System.Environment
import System.Exit
import System.IO

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <number>"
    exitFailure

process :: Int -> Int
process 1 = 0
process n =
    let k = ceiling ((sqrt (fromIntegral n) - 1) / 2)
        sideLen = 2 * k + 1
        maxVal = sideLen * sideLen
        step = sideLen - 1
        d = maxVal - n
        offset = d `mod` step
    in k + abs (offset - k)

main :: IO ()
main = do
    args <- getArgs
    progname <- getProgName
    case args of
        [n] -> do
            let result = process (read n)
            putStrLn $ "result = " ++ show result
        _ -> usage progname
