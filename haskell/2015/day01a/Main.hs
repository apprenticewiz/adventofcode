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
process content = fromIntegral $ foldl count 0 content
    where count acc '(' = acc + 1
          count acc ')' = acc - 1
          count acc _ = acc

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
