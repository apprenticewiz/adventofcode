module Main ( main ) where

import System.Environment
import System.Exit
import System.IO

numRows :: Int
numRows = 40

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

process :: String -> Int
process content =
    let firstRow = head (lines content)
        rows = take numRows $ iterate step firstRow
    in countSafeTiles rows
    where
        step row =
            let paddedRow = '.' : row ++ "."
            in [ isSafeTile (paddedRow !! (i - 1), paddedRow !! i, paddedRow !! (i + 1)) | i <- [1 .. length row] ]
        isSafeTile (l, c, r)
            | l == '^' && c == '^' && r == '.' = '^'
            | l == '.' && c == '^' && r == '^' = '^'
            | l == '^' && c == '.' && r == '.' = '^'
            | l == '.' && c == '.' && r == '^' = '^'
            | otherwise = '.'
        countSafeTiles rows = sum $ map (length . filter (== '.')) rows

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
