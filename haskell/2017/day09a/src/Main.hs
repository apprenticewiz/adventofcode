module Main ( main ) where

import System.Environment
import System.Exit
import System.IO

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

process :: String -> Int
process = go False 0 0
    where
        go :: Bool -> Int -> Int -> String -> Int
        go _ _ score [] = score
        go garbage level score (x:xs) =
            if garbage
                then (case x of
                    '>' -> go False level score xs
                    '!' -> go True level score (drop 1 xs)
                    _   -> go True level score xs)
                else (case x of
                    '{' -> go False (level + 1) score xs
                    '}' -> go False (level - 1) (score + level) xs
                    '<' -> go True level score xs
                    _   -> go False level score xs)

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
