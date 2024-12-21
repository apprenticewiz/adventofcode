{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main ( main ) where

import Data.Array (Array)
import qualified Data.Array as Array
import Data.Text ( pack, splitOn, unpack )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure )

type Position = (Int, Int)

usage :: IO ()
usage = do
    progname <- getProgName
    putStrLn $ "usage: " ++ progname ++ " <file> <width> <height>"
    exitFailure

parseInput :: String -> [Position]
parseInput contents =
    foldl
        (\acc pairTxt ->
            let nums = map ( read . unpack) $ splitOn (pack ",") (pack pairTxt)
            in acc ++ [(head nums, nums !! 1)]
        )
        []
        (lines contents)

findPath :: Int -> Int -> [Position] -> Int
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

findBlocker :: Int -> Int -> Int -> [Position] -> Position
findBlocker numBytes width height !bytes
    | numBytes == length bytes = (-1, -1)
    | otherwise =
        let byteSlice = take numBytes bytes
            steps = findPath width height byteSlice
        in if steps == (maxBound :: Int)
            then bytes !! (numBytes - 1)
            else findBlocker (numBytes + 1) width height bytes

process :: String -> Int -> Int -> Position
process contents width height =
    let bytes = parseInput contents
    in findBlocker 1 width height bytes

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename, width, height] -> do
            contents <- readFile filename
            let result = process contents (read width) (read height)
            putStrLn $ "result = " ++ show result
        _ -> usage
