module Main ( main ) where

import System.Environment
import System.Exit
import System.IO
import Control.DeepSeq
import System.Clock

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
            let result = process (read n)
            result `deepseq` return ()
            end <- getTime Monotonic
            let elapsed = diffTimeSpec start end
            putStrLn $ "result = " ++ show result
            putStrLn $ "elapsed time: " ++ showTime elapsed
        _ -> usage progname
