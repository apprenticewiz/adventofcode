module Main ( main ) where

import System.Environment
import System.Exit
import System.IO

type Turn = Either Int Int

process :: String -> Int
process content =
    let turns = map parseTurn (lines content)
    in snd $ foldl' step (initDial, 0) turns

  where
    initDial :: Int
    initDial = 50

    parseTurn :: String -> Turn
    parseTurn ('L':n) = Left (read n)
    parseTurn ('R':n) = Right (read n)
    parseTurn _ = error "malformed input: expected L<n> or R<n>"

    step :: (Int, Int) -> Turn -> (Int, Int)
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
