module Main ( main ) where

import System.Environment
import System.Exit
import System.IO
import Control.DeepSeq
import System.Clock

numRows :: Int
numRows = 400000

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

process :: String -> Int
process content =
    let firstRow = head (lines content)
        rows = take numRows $ iterate step firstRow
    in countSafeTiles rows
    where
        step row =
            let paddedRow = '.' : row ++ "."
            in [ isSafeTile (paddedRow !! (i - 1), paddedRow !! i, paddedRow !! (i + 1)) | i <- [1 .. length row] ]
        isSafeTile (l, c, r)
            | l == '^' && c == '^' && r == '.' = '^'
            | l == '.' && c == '^' && r == '^' = '^'
            | l == '^' && c == '.' && r == '.' = '^'
            | l == '.' && c == '.' && r == '^' = '^'
            | otherwise = '.'
        countSafeTiles rows = sum $ map (length . filter (== '.')) rows


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
