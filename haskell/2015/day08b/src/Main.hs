module Main ( main ) where

import Data.Int (Int32)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

encodedLength :: String -> Int
encodedLength line =
    2 + sum (map encodeChar line)
    where
        encodeChar '"' = 2
        encodeChar '\\' = 2
        encodeChar _ = 1

process :: String -> Int32
process content =
    let contentLines = lines content
        codeLens = map length contentLines
        encLens = map encodedLength contentLines
        diffs = zipWith (-) encLens codeLens
    in fromIntegral $ sum diffs

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
