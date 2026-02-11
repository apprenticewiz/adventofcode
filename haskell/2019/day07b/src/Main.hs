module Main ( main ) where

import Control.DeepSeq
import Data.Maybe ( isNothing )
import Data.List ( permutations )
import System.Clock
import System.Environment
import System.Exit
import System.IO

import IntCode

process :: String -> Int
process content =
    let initMem = map read (words (map (\c -> if c == ',' then ' ' else c) content))
    in maximum [ runFeedbackLoop initMem phases | phases <- permutations [5..9] ]

runFeedbackLoop :: [Int] -> [Int] -> Int
runFeedbackLoop initMem phases =
    let amps0 = [ initCpu initMem [p] | p <- phases ]
        go amps signal lastThruster =
            let (amps', signal', lastThruster') = foldl' step (amps, signal, lastThruster) [0..4]
            in if all isNothing amps'
                then lastThruster'
                else go amps' signal' lastThruster'
        step (amps, signal, thruster) i =
            case amps !! i of
                Nothing -> (amps, signal, thruster)
                Just cpu -> case execute (cpu { inputs = inputs cpu ++ [signal] }) of
                    Halted outs ->
                        let lastOut = if null outs then signal else last outs
                        in (take i amps ++ [Nothing] ++ drop (i + 1) amps, lastOut, if i == 4 then lastOut else thruster)
                    NeedInput cpu' outs ->
                        let lastOut = if null outs then signal else last outs
                        in (take i amps ++ [Just cpu'] ++ drop (i + 1) amps, lastOut, if i == 4 then lastOut else thruster)
    in go (map Just amps0) 0 0

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
