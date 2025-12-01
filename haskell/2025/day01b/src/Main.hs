module Main ( main ) where

import System.Environment
import System.Exit
import System.IO

process :: String -> Int
process content =
    let turns = map parseTurn (lines content)
    in snd $ foldl' step (50, 0) turns

  where
    parseTurn :: String -> Int
    parseTurn ('L':n) = negate (read n)
    parseTurn ('R':n) = read n
    parseTurn _ = error "malformed input: expected L<n> or R<n>"

    step :: (Int, Int) -> Int -> (Int, Int)
    step (dial, zeros) delta =
        let newDial = (dial + delta) `mod` 100
            crossings = if delta >= 0
                        then (dial + delta) `div` 100 - dial `div` 100
                        else (dial - 1) `div` 100 - (dial + delta - 1) `div` 100
        in (newDial, zeros + crossings)

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

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
