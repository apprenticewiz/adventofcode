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
    go 0 1 [0]
  where
    go pos n buf
        | n > insertions = 
            let nextPos = (pos + 1) `mod` length buf
            in buf !! nextPos
        | otherwise =
            let pos' = (pos + step) `mod` length buf + 1
                buf' = take pos' buf ++ [n] ++ drop pos' buf
            in go pos' (n + 1) buf'

main :: IO ()
main = do
    args <- getArgs
    progname <- getProgName
    case args of
        [step, insertions] -> do
            let result = process (read step) (read insertions)
            putStrLn $ "result = " ++ show result
        _ -> usage progname
