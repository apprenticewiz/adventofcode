module Main ( main ) where

import Control.DeepSeq
import Data.List ( permutations )
import System.Clock
import System.Environment
import System.Exit
import System.IO

import IntCode

process :: String -> Int
process content =
    let initMem = map read (words (map (\c -> if c == ',' then ' ' else c) content))
    in maximum [ checkPhases initMem phases | phases <- permutations [0..4] ]
  where
    checkPhases :: [Int] -> [Int] -> Int
    checkPhases initMem phases = go initMem phases 0
      where
        go :: [Int] -> [Int] -> Int -> Int
        go _ [] input = input
        go mem (phase:rest) input =
            let cpu0 = initCpu mem [phase, input]
                outputs = execute cpu0
            in case outputs of
                output:_ -> go mem rest output
                [] -> error "no outputs"

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
