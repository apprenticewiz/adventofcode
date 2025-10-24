module Main ( main ) where

import Data.Range
import System.Environment
import System.Exit
import System.IO

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <filename>"
    exitFailure

process :: String -> Int
process content =
    let ranges = map parseRange (lines content)
        blocked = joinRanges ranges
        total = [0 +=+ (2 ^ 32 - 1)]
        allowed = difference total blocked
    in head $ fromRanges allowed
    where
        parseRange :: String -> Range Int
        parseRange str =
            let (startStr, '-':endStr) = span (/= '-') str
                start = read startStr :: Int
                end = read endStr :: Int
            in start +=+ end

main :: IO ()
main = do
    args <- getArgs
    progname <- getProgName
    case args of
        [filename] -> do
            content <- readFile filename
            let result = process content
            putStrLn $ "result = " ++ show result
        _ -> usage progname
