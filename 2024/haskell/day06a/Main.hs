module Main ( main ) where

import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Set ( Set )
import qualified Data.Set as Set
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure )

usage :: IO ()
usage = do
  progname <- getProgName
  putStrLn $ "usage: " ++ progname ++ " <file>"
  exitFailure

process :: String -> Int
process contents =
    let grid = lines contents
        numRows = length grid
        numCols = length (head grid)
        extents = (numRows, numCols)
        charAt g (r, c) = (g !! r) !! c
        startPos = head $ filter (\(r, c) -> charAt grid (r, c) == '^')
            [ (row, col) | row <- [0..(numRows - 1)], col <- [0..(numCols - 1)]]
        startDir = (-1, 0)
        obstacles = Set.fromList $ filter (\(r, c) -> charAt grid (r, c) == '#')
            [ (row, col) | row <- [0..(numRows - 1)], col <- [0..(numCols - 1)]]
        turn = Map.fromList [((-1, 0), (0, 1)), ((0, 1), (1, 0)), ((1, 0), (0, -1)), ((0, -1), (-1, 0))]
        inBounds (r, c) = r >= 0 && r < numRows && c >= 0 && c < numCols
        walk pos dir obs path =
            let (r, c) = pos
                (dr, dc) = dir
                (nr, nc) = (r + dr, c + dc)
            in if not (inBounds (nr, nc))
                then path ++ [(pos, dir)]
                else if Set.member (nr, nc) obs
                    then walk pos (turn Map.! dir) obs path
                    else walk (nr, nc) dir obs (path ++ [(pos, dir)])
        path = walk startPos startDir obstacles []
        positions = Set.fromList $ map fst path
    in Set.size positions

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      contents <- readFile filename
      let result = process contents
      putStrLn $ "result = " ++ show result
    _ -> usage
