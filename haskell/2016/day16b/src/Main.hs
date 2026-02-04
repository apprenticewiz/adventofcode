module Main ( main ) where

import Control.DeepSeq
import System.Clock
import System.Environment
import System.Exit
import System.IO

diskLen :: Int
diskLen = 35651584

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input data>"
    exitFailure

process :: String -> String
process inputData =
    let filledData = expandData inputData
    in computeChecksum (take diskLen filledData)
    where
        expandData a
            | length a >= diskLen = a
            | otherwise =
                let b = reverse (map flipBit a)
                in expandData (a ++ "0" ++ b)
        flipBit '0' = '1'
        flipBit '1' = '0'
        flipBit _   = error "Invalid character in input data"
        computeChecksum xs
            | odd (length xs) = xs
            | otherwise = computeChecksum (pairwiseChecksum xs)
        pairwiseChecksum [] = []
        pairwiseChecksum [_] = []
        pairwiseChecksum (x:y:rest)
            | x == y    = '1' : pairwiseChecksum rest
            | otherwise = '0' : pairwiseChecksum rest

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
        [inputData] -> do
            start <- getTime Monotonic
            let result = process inputData
            result `deepseq` return ()
            end <- getTime Monotonic
            let elapsed = diffTimeSpec start end
            putStrLn $ "result = " ++ result
            putStrLn $ "elapsed time: " ++ showTime elapsed
        _ -> usage progname
