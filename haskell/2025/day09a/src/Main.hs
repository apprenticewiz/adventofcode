module Main ( main ) where

import Control.DeepSeq
import System.Clock
import System.Environment
import System.Exit
import System.IO

type Position = (Int, Int)

process :: String -> Int
process content =
    let points = map parsePoint (lines content)
        pairs = [ (p1, p2) | p1 <- points, p2 <- points, p1 < p2 ]
    in maximum $ map (uncurry area) pairs

  where
    parsePoint :: String -> Position
    parsePoint line =
        let (xStr, _:yStr) = span (/= ',') line
        in (read xStr, read yStr)

    area :: Position -> Position -> Int
    area (x1, y1) (x2, y2) = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)

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
       else if ns < 10000000000
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
