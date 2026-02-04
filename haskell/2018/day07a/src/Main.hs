module Main ( main ) where

import Control.DeepSeq
import Data.List
import System.Clock
import System.Environment
import System.Exit
import System.IO

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

process :: String -> String
process contents =
    let steps = map parseStep (lines contents)
        q = (nub . sort) [ a | (a, _) <- steps, not (any (\(_, b) -> a == b) steps) ]
    in go steps q []
  where
    parseStep :: String -> (Char, Char)
    parseStep line =
        let ws = words line
        in (head (ws !! 1), head (ws !! 7))

    go :: [(Char, Char)] -> [Char] -> String -> String
    go _     [] result = result
    go steps q  result =
        let x = head q
            result' = result ++ [x]
            candidates = [ b | (a, b) <- steps, a == x ]
            ready = [ c | c <- candidates
                        , let prereqs = map fst $ filter (\(a, b) -> b == c) steps
                        , all (`elem` result') prereqs
                    ]
            q' = (nub . sort) (tail q ++ ready)
        in go steps q' result'

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
