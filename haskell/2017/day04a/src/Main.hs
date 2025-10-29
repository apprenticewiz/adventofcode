module Main ( main ) where

import Data.List
import Data.Maybe
import System.Environment
import System.Exit
import System.IO

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

process :: String -> Int
process content = (length . mapMaybe valid) (lines content)
    where
        valid :: String -> Maybe String
        valid line =
            let parts = words line
            in if length parts == length (nub parts)
               then Just line
               else Nothing

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
