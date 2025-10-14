module Main ( main ) where

import Data.List
import qualified Data.Map.Strict as Map
import Data.Ord
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
        width = length (ls !! 0)
        histos = foldl (\cols line -> map (\(col, ch) -> Map.insertWith (+) ch 1 col) (zip cols line))
                       (replicate width Map.empty)
                       ls
    in map (fst . minimumBy (comparing snd) . Map.assocs) histos

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
