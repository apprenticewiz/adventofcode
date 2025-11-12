module Main ( main ) where

import Data.List
import System.Environment
import System.Exit
import System.IO

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

process :: String -> String
process contents =
    let steps = map parseStep (lines contents)
        q = (nub . sort) [ a | (a, _) <- steps, not (any (\(_, b) -> a == b) steps) ]
    in go steps q []
  where
    parseStep :: String -> (Char, Char)
    parseStep line =
        let ws = words line
        in (head (ws !! 1), head (ws !! 7))

    go :: [(Char, Char)] -> [Char] -> String -> String
    go _     [] result = result
    go steps q  result =
        let x = head q
            result' = result ++ [x]
            candidates = [ b | (a, b) <- steps, a == x ]
            ready = [ c | c <- candidates
                        , let prereqs = map fst $ filter (\(a, b) -> b == c) steps
                        , all (`elem` result') prereqs
                    ]
            q' = (nub . sort) (tail q ++ ready)
        in go steps q' result'

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
