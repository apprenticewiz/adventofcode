module Main ( main ) where

import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Data.Set ( Set )
import qualified Data.Set as Set
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure )

type Position = (Int, Int)

type Direction = (Int, Int)

data Status a = Blocked | Move a
    deriving (Eq, Show)

usage :: IO ()
usage = do
    progname <- getProgName
    putStrLn $ "usage: " ++ progname ++ " <file>"
    exitFailure

directionsMap :: Map Char Direction
directionsMap = Map.fromList [('^', (-1, 0)), ('v', (1, 0)), ('<', (0, -1)), ('>', (0, 1))]

scanGrid :: [String] -> ((Int, Int), Position, Set (Position, Position), Set (Position, Position))
scanGrid grid =
    let height = length grid
        width = 2 * length (head grid)
        robot = head [(r, 2 * c) | (r, row) <- zip [0..] grid,
                                   (c, cell) <- zip [0..] row,
                                   cell == '@']
        boxes = Set.fromList [((r, 2 * c), (r, 2 * c + 1)) | (r, row) <- zip [0..] grid,
                                                             (c, cell) <- zip [0..] row,
                                       cell == 'O']
        obstacles = Set.fromList [((r, 2 * c), (r, 2 * c + 1)) | (r, row) <- zip [0..] grid,
                                                                 (c, cell) <- zip [0..] row,
                                           cell == '#']
    in ((height, width), robot, boxes, obstacles)

splitInput :: [String] -> ([String], String)
splitInput contentLines = (takeWhile (/= "") contentLines, filter (/= '\n') $ unlines $ dropWhile (/= "") contentLines)

moveRobot :: Char -> Position -> Set (Position, Position) -> Set (Position, Position) -> (Position, Set (Position, Position))
moveRobot dir (x, y) boxes obstacles =
    let (dx, dy) = case Map.lookup dir directionsMap of
                        Just delta -> delta
                        Nothing -> error ("unexpected character in input: " ++ show dir)
        overlaps (x, y) objects = any (\((x1, y1), (x2, y2)) -> (x, y) == (x1, y1) || (x, y) == (x2, y2))
            (Set.toList objects)
        getOverlap (x, y) objects = Set.fromList (filter (\((x1, y1), (x2, y2)) -> (x, y) == (x1, y1) || (x, y) == (x2, y2))
            (Set.toList objects))
        scanBoxes (x, y) (dx, dy) workingSet =
            if Set.null workingSet
                then let (nx, ny) = (x + dx, y + dy)
                     in if overlaps (nx, ny) boxes
                            then scanBoxes (x, y) (dx, dy) (Set.union workingSet (getOverlap (nx, ny) boxes))
                            else if overlaps (nx, ny) obstacles
                                     then Blocked
                                     else Move workingSet
                else let checkNext = Set.difference boxes workingSet
                         addedBoxes =
                            foldl
                                (\newSet ((x1, y1), (x2, y2)) ->
                                    if overlaps (x1 + dx, y1 + dy) checkNext
                                        then Set.union newSet (getOverlap (x1 + dx, y1 + dy) checkNext)
                                        else if overlaps (x2 + dx, y2 + dy) checkNext
                                                then Set.union newSet (getOverlap (x2 + dx, y2 + dy) checkNext)
                                                else newSet
                                )
                                Set.empty
                                (Set.toList workingSet)
                         newWorkingSet = Set.union workingSet addedBoxes
                     in if newWorkingSet /= workingSet
                            then scanBoxes (x, y) (dx, dy) newWorkingSet
                            else if any (\((x1, y1), (x2, y2)) -> overlaps (x1 + dx, y1 + dy) obstacles || overlaps (x2 + dx, y2 + dy) obstacles) newWorkingSet
                                    then Blocked
                                    else Move newWorkingSet
        applyMove (x, y) (dx, dy) moveStatus =
            case moveStatus of
                Blocked -> ((x, y), boxes)
                Move boxesToMove -> 
                    let boxesNotMoved = Set.difference boxes boxesToMove
                        newBoxes = Set.fromList $ map (\((bx1, by1), (bx2, by2)) -> ((bx1 + dx, by1 + dy), (bx2 + dx, by2 + dy)))
                                        (Set.toList boxesToMove)
                     in ((x + dx, y + dy), Set.union boxesNotMoved newBoxes)
    in applyMove (x, y) (dx, dy) (scanBoxes (x, y) (dx, dy) Set.empty)

processMoves :: String -> Position -> Set (Position, Position) -> Set (Position, Position) -> (Position, Set (Position, Position))
processMoves [] robot boxes _ = (robot, boxes)
processMoves (dir:rest) robot boxes obstacles =
    let (robot', boxes') = moveRobot dir robot boxes obstacles
    in processMoves rest robot' boxes' obstacles

calcGPS :: (Position, Position) -> Int
calcGPS ((r1, c1), _) = (100 * r1) + c1

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
