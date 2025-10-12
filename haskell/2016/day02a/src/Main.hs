module Main ( main ) where

import qualified Data.Map.Strict as Map
import System.Environment
import System.Exit
import System.IO

data Direction = U | D | L | R
                 deriving (Eq, Ord, Show)

type Transitions = Map.Map Int (Map.Map Direction Int)

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

transitions :: Transitions
transitions = Map.fromList [ (1, Map.fromList [ (U, 1), (D, 4), (L, 1), (R, 2) ])
                           , (2, Map.fromList [ (U, 2), (D, 5), (L, 1), (R, 3) ])
                           , (3, Map.fromList [ (U, 3), (D, 6), (L, 2), (R, 3) ])
                           , (4, Map.fromList [ (U, 1), (D, 7), (L, 4), (R, 5) ])
                           , (5, Map.fromList [ (U, 2), (D, 8), (L, 4), (R, 6) ])
                           , (6, Map.fromList [ (U, 3), (D, 9), (L, 5), (R, 6) ])
                           , (7, Map.fromList [ (U, 4), (D, 7), (L, 7), (R, 8) ])
                           , (8, Map.fromList [ (U, 5), (D, 8), (L, 7), (R, 9) ])
                           , (9, Map.fromList [ (U, 6), (D, 9), (L, 8), (R, 9) ])
                           ]

process :: String -> Int
process content =
    fst $ foldl (\(final, code) line ->
                   let dirs = map charToDir line
                       code' = foldl (\acc dir -> (transitions Map.! acc) Map.! dir) code dirs
                   in (final * 10 + code', code')
                )
                (0, 5)
                (lines content)
  where
    charToDir :: Char -> Direction
    charToDir ch = case ch of
        'U' -> U
        'D' -> D
        'L' -> L
        'R' -> R
        _   -> error "charToDir: expected one of U, D, L, or R"

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
