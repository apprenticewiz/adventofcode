module Main ( main ) where

import System.Environment
import System.Exit
import System.IO

data Direction = L Int | R Int
                 deriving (Eq, Show)

process :: String -> Int
process content =
    let turns = map parseTurn (lines content)
        (_, zeros) = foldl' step (50, 0) turns
    in zeros
  where
    parseTurn :: String -> Direction
    parseTurn s =
        case s of
            'L' : n -> L (read n)
            'R' : n -> R (read n)
            _ -> error "malformed input: expected L<n> or R<n>"

    step :: (Int, Int) -> Direction -> (Int, Int)
    step (dial, zeros) turn =
        let dial' = case turn of
                        L n -> (dial - n) `mod` 100
                        R n -> (dial + n) `mod` 100
            zeros' = if dial' == 0 then zeros + 1 else zeros
        in (dial', zeros')

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
