module Main ( main ) where

import System.Environment
import System.Exit
import System.IO

process :: String -> Int
process content =
    let turns = map parseTurn (lines content)
        (_, zeros) = foldl' step (50, 0) turns
    in zeros

  where
    parseTurn :: String -> Either Int Int
    parseTurn s =
        case s of
            'L' : n -> Left (read n)
            'R' : n -> Right (read n)
            _ -> error "malformed input: expected L<n> or R<n>"

    step :: (Int, Int) -> Either Int Int -> (Int, Int)
    step (dial, zeros) turn 
        | turn == Left 0 || turn == Right 0 = (dial, zeros)
        | dial == 0 = case turn of
            Left n -> step ((dial - 1) `mod` 100, zeros + 1) (Left (n - 1))
            Right n -> step ((dial + 1) `mod` 100, zeros + 1) (Right (n - 1))
        | otherwise = case turn of
            Left n -> step ((dial - 1) `mod` 100, zeros) (Left (n - 1))
            Right n -> step ((dial + 1) `mod` 100, zeros) (Right (n - 1))

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
