module Main ( main ) where

import Data.Int (Int32)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Control.DeepSeq
import System.Clock

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

memLength :: String -> Int
memLength line =
    let betweenQuotes = (init . drop 1) line
        countEscaped i [] = i
        countEscaped i [_] = i + 1
        countEscaped i (x:y:xs) =
            case [x, y] of
                "\\\\" -> countEscaped (i + 1) xs
                "\\\"" -> countEscaped (i + 1) xs
                "\\x" -> countEscaped (i + 1) (drop 2 xs)
                _ -> countEscaped (i + 1) (y:xs)
    in countEscaped 0 betweenQuotes

process :: String -> Int32
process content =
    let contentLines = lines content
        codeLens = map length contentLines
        memLens = map memLength contentLines
        diffs = zipWith (-) codeLens memLens
    in fromIntegral $ sum diffs


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
