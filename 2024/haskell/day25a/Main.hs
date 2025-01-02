module Main ( main ) where

import Data.Array ( Array )
import qualified Data.Array as Array
import Data.List ( break )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure )

usage :: IO ()
usage = do
    progname <- getProgName
    putStrLn $ "usage: " ++ progname ++ " <file>"
    exitFailure

parse :: [String] -> ([[Int]], [[Int]])
parse = parse' ([], [])
    where
        countPins grid =
            let (_, (maxRow, maxCol)) = Array.bounds grid
                cols = map (\c -> [grid Array.! (r, c) | r <- [1..(maxRow - 1)]]) [0..maxCol]
            in [map (length . filter (== '#')) cols]
        parse' (locks, keys) [] = (locks, keys)
        parse' (locks, keys) xs =
            let (currentItem, rest) = break (== "") xs
                numRows = length currentItem
                numCols = length (head currentItem)
                bounds = ((0, 0), (numRows - 1, numCols - 1))
                elements = [((r, c), currentItem !! r !! c) | r <- [0..(numRows - 1)], c <- [0..(numCols - 1)]]
                grid = Array.array bounds elements
                topRow = [grid Array.! (0, c) | c <- [0..(numCols - 1)]]
            in if all (== '#') topRow
                then parse' (locks ++ countPins grid, keys) (drop 1 rest)
                else parse' (locks, keys ++ countPins grid) (drop 1 rest)

process :: String -> Int
process contents =
    let (locks, keys) = parse (lines contents)
    in foldl (\count lock -> count + length (filter (all (<= 5)) $ map (zipWith (+) lock) keys)) 0 locks

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            contents <- readFile filename
            let result = process contents
            putStrLn $ "result = " ++ show result
        _ -> usage
