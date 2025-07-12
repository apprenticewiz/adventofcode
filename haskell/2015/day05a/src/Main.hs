module Main ( main ) where

import Data.List (any, elemIndices, isInfixOf)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, readFile, stderr)
import Data.Int (Int32)

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

prop1 :: String -> Bool
prop1 line =
    let vowels = "aeiou"
        numVowels = sum $ map (\x -> length (x `elemIndices` line)) vowels
    in numVowels >= 3

prop2 :: String -> Bool
prop2 line =
    let doubles = [ [x, x] | x <- ['a'..'z'] ]
    in any (`isInfixOf` line) doubles

prop3 :: String -> Bool
prop3 line =
    let naughties = ["ab", "cd", "pq", "xy"]
    in not (any (`isInfixOf` line) naughties)

process :: String -> Int32
process content =
    let contentLines = lines content
    in sum $ map (\x -> if prop1 x && prop2 x && prop3 x then 1 else 0) contentLines

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
