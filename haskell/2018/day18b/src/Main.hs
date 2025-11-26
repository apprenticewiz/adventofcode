module Main ( main ) where

import Data.Array.Unboxed
import qualified Data.Map.Strict as Map
import System.Environment
import System.Exit
import System.IO

type Grid = UArray (Int, Int) Char

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

step :: Grid -> Grid
step grid = listArray (bounds grid)
    [ ch' | (x, y) <- indices grid
          , let ch' = case grid ! (x, y) of
                    '.' -> if countNeighbors grid (x, y) '|' >= 3 then '|' else '.'
                    '|' -> if countNeighbors grid (x, y) '#' >= 3 then '#' else '|'
                    '#' -> if countNeighbors grid (x, y) '#' >= 1 && countNeighbors grid (x, y) '|' >= 1 then '#' else '.'
                    _ -> error "unexpected character in grid"
    ]

gridToKey :: Grid -> [Char]
gridToKey = elems

findCycleAndProject :: Int -> Grid -> Grid
findCycleAndProject target initial = go 0 initial Map.empty
  where
    go iter grid seen
        | iter == target = grid
        | otherwise =
            let key = gridToKey grid
            in case Map.lookup key seen of
                Just cycleStart ->
                    let cycleLength = iter - cycleStart
                        remaining = target - iter
                        stepsInCycle = remaining `mod` cycleLength
                    in iterate step grid !! stepsInCycle
                Nothing ->
                    let nextGrid = step grid
                    in go (iter + 1) nextGrid (Map.insert key iter seen)

countNeighbors :: Grid -> (Int, Int) -> Char -> Int
countNeighbors grid (x, y) ch = length [ () | (dx, dy) <- [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)],
                                         let (x', y') = (x + dx, y + dy),
                                         inRange (bounds grid) (x', y'),
                                         grid ! (x', y') == ch
                                       ]

countTrees :: Grid -> Int
countTrees grid = length [ () | (_, ch) <- assocs grid, ch == '|' ]

countLumberyards :: Grid -> Int
countLumberyards grid = length [ () | (_, ch) <- assocs grid, ch == '#' ]

process :: String -> Int
process content =
    let ls = lines content
        maxX = length (head ls) - 1
        maxY = length ls - 1
        grid0 = listArray ((0, 0), (maxX, maxY)) (concat ls)
        finalGrid = findCycleAndProject 1000000000 grid0
        trees = countTrees finalGrid
        lumberyards = countLumberyards finalGrid
    in trees * lumberyards

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
