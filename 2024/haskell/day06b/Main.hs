module Main ( main ) where

import Data.List ( elemIndices )
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
        walk pos dir obs checkCycles path =
            let (r, c) = pos
                (dr, dc) = dir
                (nr, nc) = (r + dr, c + dc)
            in if not (inBounds (nr, nc)) || (checkCycles && (pos, dir) `elem` path)
                then path ++ [(pos, dir)]
                else if Set.member (nr, nc) obs
                    then walk pos (turn Map.! dir) obs checkCycles path
                    else walk (nr, nc) dir obs checkCycles (path ++ [(pos, dir)])
        path = walk startPos startDir obstacles False []
        tryObstacle pos dir newObstacle =
            let newObstacles = Set.insert newObstacle obstacles
                simPath = walk pos dir newObstacles True []
                lastState = last simPath
            in length (elemIndices lastState simPath) > 1
        addObstacles [move] obstacleSet = obstacleSet
        addObstacles (move:nextMove:rest) obstacleSet =
            let (pos, dir) = move
                newObstacle = fst nextMove
                result = tryObstacle startPos startDir newObstacle
            in if result
                then addObstacles (nextMove:rest) (Set.insert newObstacle obstacleSet)
                else addObstacles (nextMove:rest) obstacleSet
        resultSet = addObstacles path Set.empty
    in Set.size resultSet

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      contents <- readFile filename
      let result = process contents
      putStrLn $ "result = " ++ show result
    _ -> usage
