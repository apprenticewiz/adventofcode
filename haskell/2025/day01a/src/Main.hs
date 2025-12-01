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
    step (dial, zeros) turn =
        let dial' = case turn of
                        Left n -> (dial - n) `mod` 100
                        Right n -> (dial + n) `mod` 100
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
