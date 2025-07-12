module Main ( main ) where

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, readFile, stderr)
import Data.Int (Int32)

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn delimiter (x:xs)
    | x == delimiter = "":rest
    | otherwise      = (x:head rest):tail rest
    where
        rest = splitOn delimiter xs

process :: String -> Int32
process content = foldl processLines 0 (lines content)
    where
        processLines acc line =
            let dimensions = map read $ splitOn 'x' line
                area1 = (dimensions !! 0) * (dimensions !! 1)
                area2 = (dimensions !! 0) * (dimensions !! 2)
                area3 = (dimensions !! 1) * (dimensions !! 2)
                surfaceArea = (area1 * 2) + (area2 * 2) + (area3 * 2)
                minArea = minimum [area1, area2, area3]
            in acc + surfaceArea + minArea

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
