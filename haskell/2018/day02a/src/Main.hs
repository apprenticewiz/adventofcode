module Main ( main ) where

import Data.List
import System.Environment
import System.Exit
import System.IO

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

process :: String -> Int
process content =
    let ls = lines content
        twos = length [ l | l <- ls, any (\x -> length x == 2) $ group $ sort l ]
        threes = length [ l | l <- ls, any (\x -> length x == 3) $ group $ sort l ]
    in twos * threes

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
