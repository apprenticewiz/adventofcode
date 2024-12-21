{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main ( main ) where

import Data.Array (Array)
import qualified Data.Array as Array
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text ( pack, splitOn, unpack )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure )

type Position = (Int, Int)

usage :: IO ()
usage = do
    progname <- getProgName
    putStrLn $ "usage: " ++ progname ++ " <file> <width> <height> <number of bytes>"
    exitFailure

parseInput :: String -> Int -> Set Position
parseInput contents numBytes =
    Set.fromList $ take numBytes $ foldl
        (\acc pairTxt ->
            let nums = map ( read . unpack) $ splitOn (pack ",") (pack pairTxt)
            in acc ++ [(head nums, nums !! 1)]
        )
        []
        (lines contents)

findPath :: Int -> Int -> Set Position -> Int
findPath width height bytes =
    let start = (0, 0)
        end = (width - 1, height - 1)
        buildMap !q !grid
            | null q = grid
            | otherwise =
                let (steps, position@(x, y)) = head q
                in if steps < grid Array.! (x, y)
                    then let
                            newGrid = grid Array.// [((x, y), steps)]
                            directions = [(0, -1), (1, 0), (0, 1), (-1, 0)]
                            isValid grid (x, y) = x >= 0 && x < width && y >= 0 && y < width
                            neighbors = filter (isValid grid) $ [(x + dx, y + dy) | (dx, dy) <- directions]
                            newEntries = map (\x -> (steps + 1, x)) neighbors
                            newQ = tail q ++ newEntries
                         in buildMap newQ newGrid
                    else buildMap (tail q) grid
        grid = buildMap [(0, start)] (Array.array ((0, 0), (width - 1, height - 1))
                    [if (x, y) `elem` bytes then ((x, y), -1) else ((x, y), maxBound :: Int) | x <- [0..(width - 1)], y <- [0..(height - 1)]])
    in (grid Array.! end)

process :: String -> Int -> Int -> Int -> Int
process contents width height numBytes =
    let bytes = parseInput contents numBytes
    in findPath width height bytes

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename, width, height, numBytes] -> do
            contents <- readFile filename
            let result = process contents (read width) (read height) (read numBytes)
            putStrLn $ "result = " ++ show result
        _ -> usage
