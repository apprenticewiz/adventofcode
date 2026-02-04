module Main ( main ) where

import Control.DeepSeq
import System.Clock
import System.Environment
import System.Exit
import System.IO

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

decompressedLength :: String -> Int
decompressedLength [] = 0
decompressedLength ('(':xs) =
    let (marker, rest) = span (/= ')') xs
        afterMarker = drop 1 rest
        (l, _:r) = span (/= 'x') marker
        (seqLen, rep) = (read l :: Int, read r :: Int)
        (segment, remainder) = splitAt seqLen afterMarker
    in (rep * decompressedLength segment) + decompressedLength remainder
decompressedLength (_:xs) = 1 + decompressedLength xs

process :: String -> Int
process content = sum $ map decompressedLength (lines content)

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
