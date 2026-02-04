module Main ( main ) where

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, readFile, stderr)
import Data.Int (Int32)
import Control.DeepSeq
import System.Clock

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn delimiter (x:xs)
    | x == delimiter = "":rest
    | otherwise      = (x:head rest):tail rest
    where
        rest = splitOn delimiter xs

process :: String -> Int32
process content = foldl processLines 0 (lines content)
    where
        processLines acc line =
            let dimensions = map read $ splitOn 'x' line
                perim1 = ((dimensions !! 0) + (dimensions !! 1)) * 2
                perim2 = ((dimensions !! 0) + (dimensions !! 2)) * 2
                perim3 = ((dimensions !! 1) + (dimensions !! 2)) * 2
                presentLen = minimum [perim1, perim2, perim3]
                bowLen = (dimensions !! 0) * (dimensions !! 1) * (dimensions !! 2)
            in acc + presentLen + bowLen


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
