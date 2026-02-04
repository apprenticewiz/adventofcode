module Main ( main ) where

import Data.Range
import System.Environment
import System.Exit
import System.IO
import Control.DeepSeq
import System.Clock

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <filename>"
    exitFailure

process :: String -> Int
process content =
    let ranges = map parseRange (lines content)
        blocked = joinRanges ranges
        total = [0 +=+ (2 ^ 32 - 1)]
        allowed = difference total blocked
    in length $ fromRanges allowed
    where
        parseRange :: String -> Range Int
        parseRange str =
            let (startStr, '-':endStr) = span (/= '-') str
                start = read startStr :: Int
                end = read endStr :: Int
            in start +=+ end


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
