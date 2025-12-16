module Main ( main ) where

import Control.DeepSeq
import System.Clock
import System.Environment
import System.Exit
import System.IO

process :: String -> Int
process content =
    let turns = map parseTurn (lines content)
    in snd $ foldl' step (50, 0) turns

  where
    parseTurn :: String -> Int
    parseTurn ('L':n) = negate (read n)
    parseTurn ('R':n) = read n
    parseTurn _ = error "malformed input: expected L<n> or R<n>"

    step :: (Int, Int) -> Int -> (Int, Int)
    step (dial, zeros) delta =
        let dial' = (dial + delta) `mod` 100
            zeros' = if dial' == 0 then zeros + 1 else zeros
        in (dial', zeros')

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
