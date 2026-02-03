module Main ( main ) where

import Control.DeepSeq
import qualified Data.Vector.Unboxed as Vector
import System.Clock
import System.Environment
import System.Exit
import System.IO

import IntCode

process :: String -> Int
process content =
    let initMem = map read (words (map (\c -> if c == ',' then ' ' else c) content))
        cpu0 = initCpu initMem
        mem1 = memory cpu0 Vector.// [(1, 12), (2, 2)]
        cpu0' = cpu0 { memory = mem1 }
        cpuFinal = execute cpu0'
    in memory cpuFinal Vector.! 0

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
