module Main ( main ) where

import Data.List ( nub )
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure )

type Position = (Int, Int)

usage :: IO ()
usage = do
    progname <- getProgName
    putStrLn $ "usage: " ++ progname ++ " <file> <threshold>"
    exitFailure

readInput :: String -> ([String], Position, Position)
readInput contents =
    let grid = lines contents
        numRows = length grid
        numCols = length (head grid)
        start = head $ [(r, c) | r <- [0..(numRows - 1)], c <- [0..(numCols - 1)], grid !! r !! c == 'S' ]
        end = head $ [(r, c) | r <- [0..(numRows - 1)], c <- [0..(numCols - 1)], grid !! r !! c == 'E' ]
    in (grid, start, end)

dfs :: [String] -> Position -> Position -> [(Position, Int)]
dfs grid start end = dfs' [(start, 0)] Set.empty []
    where dfs' !q !visited !currentPath
            | null q = currentPath
            | otherwise =
                let (pos, steps) = head q
                    rest = tail q
                    currentPath' = currentPath ++ [(pos, steps)]
                    visited' = Set.insert pos visited
                    neighbors = filter (`Set.notMember` visited) (filter isValid $ getNeighbors pos)
                in if pos == end
                    then currentPath'
                    else dfs' (nub (foldl (\tempQ pos' ->  tempQ ++ [(pos', steps + 1)]) rest neighbors)) visited' currentPath'
          numRows = length grid
          numCols = length (head grid)
          getNeighbors (row, col) = [(row + dr, col + dc) | (dr, dc) <- [(-1, 0), (0, 1), (1, 0), (0, -1)]]
          isValid (row, col) = row >= 0 && row < numRows && col >= 0 && col < numCols && grid !! row !! col /= '#'

findCheats :: [(Position, Int)] -> Int -> Int
findCheats path threshold = findCheats' path Set.empty 0
    where
        endCost = snd $ last path
        manhattanDistance ((r1, c1), _) ((r2, c2), _) = abs (r1 - r2) + abs (c1 - c2)
        calcCostWithCheat n p1 p2 = snd p1 + n + (endCost - snd p2)
        findCheats' [] _ count = count
        findCheats' (pos:rest) checked count =
            let (newChecked, newCount) =
                    foldl
                        (\(checked', count') n ->
                            let diamond = [otherPos | otherPos <- path, otherPos /= pos, manhattanDistance pos otherPos == n,
                                           (otherPos, pos) `Set.notMember` checked]
                                savings = map ((endCost -) . calcCostWithCheat n pos) diamond
                                foundCheats = length $ filter (>= threshold) savings
                                updatedCheck = Set.fromList [(pos, x) | x <- diamond]
                            in (Set.union checked' updatedCheck, count' + foundCheats)
                        )
                        (checked, count)
                        [2..20]
            in findCheats' rest newChecked newCount

process :: String -> Int -> Int
process contents threshold =
    let (grid, start, end) = readInput contents
        path = dfs grid start end
    in findCheats path threshold

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename, threshold] -> do
            contents <- readFile filename
            let result = process contents (read threshold)
            putStrLn $ "result = " ++ show result
        _ -> usage
