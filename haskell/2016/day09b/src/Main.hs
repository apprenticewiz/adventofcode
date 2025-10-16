module Main ( main ) where

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
