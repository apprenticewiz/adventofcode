module Main ( main ) where

import Data.List ( (\\), union )
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Data.Set ( Set )
import qualified Data.Set as Set
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure )

type Position = (Int, Int)

type Direction = (Int, Int)

usage :: IO ()
usage = do
    progname <- getProgName
    putStrLn $ "usage: " ++ progname ++ " <file>"
    exitFailure

directionsMap :: Map Char Direction
directionsMap = Map.fromList [('^', (-1, 0)), ('v', (1, 0)), ('<', (0, -1)), ('>', (0, 1))]

scanGrid :: [String] -> ((Int, Int), Position, Set Position, Set Position)
scanGrid grid =
    let height = length grid
        width = length (head grid)
        robot = head [(r, c) | (r, row) <- zip [0..] grid,
                               (c, cell) <- zip [0..] row,
                               cell == '@']
        boxes = Set.fromList [(r, c) | (r, row) <- zip [0..] grid,
                                       (c, cell) <- zip [0..] row,
                                       cell == 'O']
        obstacles = Set.fromList [(r, c) | (r, row) <- zip [0..] grid,
                                           (c, cell) <- zip [0..] row,
                                           cell == '#']
    in ((height, width), robot, boxes, obstacles)

splitInput :: [String] -> ([String], String)
splitInput contentLines = (takeWhile (/= "") contentLines, filter (/= '\n') $ unlines $ tail $ dropWhile (/= "") contentLines)

moveRobot :: Char -> Position -> Set Position -> Set Position -> (Position, Set Position)
moveRobot dir robot@(x, y) boxes obstacles =
    let (dx, dy) = case Map.lookup dir directionsMap of
                        Just delta -> delta
                        Nothing -> error ("unexpected character in input: " ++ show dir)
        tryMove (dx, dy) (rx, ry) movedBoxes =
            if null movedBoxes
                then let (nx, ny) = (rx + dx, ry + dy)
                     in if (nx, ny) `Set.notMember` boxes
                            then if (nx, ny) `Set.notMember` obstacles
                                    then ((nx, ny), boxes)
                                    else ((rx, ry), boxes)
                            else tryMove (dx, dy) (rx, ry) ((nx, ny):movedBoxes)
                else let (bx, by) = head movedBoxes
                         (nx, ny) = (bx + dx, by + dy)
                     in if (nx, ny) `Set.notMember` boxes
                            then if (nx, ny) `Set.notMember` obstacles
                                    then let tmpBoxes = Set.toList boxes \\ movedBoxes
                                             newPositions = [(px + dx, py + dy) | (px, py) <- movedBoxes]
                                         in ((rx + dx, ry + dy), Set.fromList (tmpBoxes `union` newPositions))
                                    else ((rx, ry), boxes)
                            else tryMove (dx, dy) (rx, ry) ((nx, ny):movedBoxes)
    in tryMove (dx, dy) (x, y) []

processMoves :: String -> Position -> Set Position -> Set Position -> (Position, Set Position)
processMoves [] robot boxes _ = (robot, boxes)
processMoves (dir:rest) robot boxes obstacles =
    let (robot', boxes') = moveRobot dir robot boxes obstacles
    in processMoves rest robot' boxes' obstacles

calcGPS :: Position -> Int
calcGPS (r, c) = (100 * r) + c

process :: String -> Int
process contents =
    let (grid, directions) = splitInput (lines contents)
        ((height, width), robot, boxes, obstacles) = scanGrid grid
        (_, finalBoxes) = processMoves directions robot boxes obstacles
    in sum $ map calcGPS $ Set.toList finalBoxes

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            contents <- readFile filename
            let result = process contents
            putStrLn $ "result = " ++ show result
        _ -> usage
