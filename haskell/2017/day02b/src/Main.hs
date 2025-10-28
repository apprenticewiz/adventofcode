module Main ( main ) where

import System.Environment
import System.Exit
import System.IO

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

process :: String -> Int
process content =
    foldr step 0 (lines content)
    where
        step line acc =
            let nums = map read (words line) :: [Int]
                divisible = head [ (x, y) | x <- nums, y <- nums, x /= y, x `mod` y == 0 ]
            in acc + uncurry div divisible

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
