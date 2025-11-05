module Main ( main ) where

import System.Environment
import System.Exit
import System.IO

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <step> <insertions>"
    exitFailure

process :: Int -> Int -> Int
process step insertions =
    go 0 1 0
  where
    go pos n valueAfter0
        | n > insertions = valueAfter0
        | otherwise =
            let pos' = (pos + step) `mod` n + 1
                valueAfter0' = if pos' == 1 then n else valueAfter0
            in go pos' (n + 1) valueAfter0'

main :: IO ()
main = do
    args <- getArgs
    progname <- getProgName
    case args of
        [step, insertions] -> do
            let result = process (read step) (read insertions)
            putStrLn $ "result = " ++ show result
        _ -> usage progname
