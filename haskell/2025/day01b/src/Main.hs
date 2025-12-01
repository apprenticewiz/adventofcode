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
        let (dial', zeros') = go (dial, zeros) turn
        in (dial', zeros')

      where
        go :: (Int, Int) -> Direction -> (Int, Int)
        go (d, z) t
            | t == L 0 || t == R 0 = (d, z)
            | d == 0 = case t of
                L n -> go ((d - 1) `mod` 100, z + 1) (L (n - 1))
                R n -> go ((d + 1) `mod` 100, z + 1) (R (n - 1))
            | otherwise = case t of
                L n -> go ((d - 1) `mod` 100, z) (L (n - 1))
                R n -> go ((d + 1) `mod` 100, z) (R (n - 1))

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
