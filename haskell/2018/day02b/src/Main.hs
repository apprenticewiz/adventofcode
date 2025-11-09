module Main ( main ) where

import Data.List
import System.Environment
import System.Exit
import System.IO

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

process :: String -> String
process content =
    let ls = lines content
        findPair [] = error "no pairs found"
        findPair (x:xs) = case [ (a, b) | a <- (x:xs), b <- xs, diffByOne a b ] of
                            (a, b):_ -> a `intersect` b
                            [] -> findPair xs
        diffByOne a b = length (filter (uncurry (/=)) (zip a b)) == 1
    in findPair ls

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
