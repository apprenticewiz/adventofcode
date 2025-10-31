module Main ( main ) where

import System.Environment
import System.Exit
import System.IO

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

process :: String -> Int
process = go False 0
    where
        go :: Bool -> Int -> String -> Int
        go _ removed [] = removed
        go garbage removed (x:xs) =
            if garbage
                then case x of
                    '>' -> go False removed xs
                    '!' -> go True removed (drop 1 xs)
                    _   -> go True (removed + 1) xs
                else case x of
                    '<' -> go True removed xs
                    _   -> go False removed xs

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
