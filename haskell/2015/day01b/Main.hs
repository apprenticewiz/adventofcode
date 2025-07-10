module Main ( main ) where

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, readFile, stderr)
import Data.Int (Int32)

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

process :: String -> Int32
process content = fromIntegral $ fst $ foldl findPosition (0, 0) content
    where findPosition (pos, count) x
            | count < 0 = (pos, count)
            | otherwise =
                let count' = case x of
                                '(' -> count + 1
                                ')' -> count - 1
                                _ -> count
                in (pos + 1, count')

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
