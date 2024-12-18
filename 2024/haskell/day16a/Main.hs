module Main ( main ) where

import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Data.Maybe ( mapMaybe, fromJust, isJust, isNothing )
import Data.PQueue.Min ( MinQueue )
import qualified Data.PQueue.Min as MinQueue
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure )

type Position = (Int, Int)
data Orientation = Vertical | Horizontal
    deriving (Eq, Ord, Show)
type Node = (Position, Orientation)
type Graph = Map Node (Map Node Int)

usage :: IO ()
usage = do
    progname <- getProgName
    putStrLn $ "usage: " ++ progname ++ " <file>"
    exitFailure

charAt :: [String] -> Position -> Char
charAt grid (row, col) = (grid !! row) !! col

findChar :: [String] -> Char -> Position
findChar grid ch = head $ [(r, c) | r <- [0..(numRows - 1)], c <- [0..(numCols - 1)], charAt grid (r, c) == ch]
    where numRows = length grid
          numCols = length (head grid)

findStart :: [String] -> Position
findStart grid = findChar grid 'S'

findEnd :: [String] -> Position
findEnd grid = findChar grid 'E'

buildGraph :: String -> Graph
buildGraph contents =
    let grid = lines contents
        numRows = length grid
        numCols = length (head grid)
    in foldl
            (\graph pos@(row, col) ->
                if charAt grid pos /= '#'
                    then
                        let dirs = [(-1, 0), (0, 1), (1, 0), (0, -1)]
                            getOrientation dir
                                | fst dir /= 0 = Vertical
                                | otherwise = Horizontal
                            tmpGraph = Map.fromList [((pos, Vertical), Map.fromList [((pos, Horizontal), 1000)]),
                                                     ((pos, Horizontal), Map.fromList [((pos, Vertical), 1000)])]
                            checkPositions = map (\(dr, dc) -> ((row + dr, col + dc), getOrientation (dr, dc))) dirs
                            newEntries = mapMaybe (\(p, o) -> if charAt grid p /= '#' then Just (p, o) else Nothing) checkPositions
                            graph' = foldl
                                        (\acc entry ->
                                            let (newPos, orientation) = entry
                                                innerMap = acc Map.! (pos, orientation)
                                                innerMap' = Map.insert (newPos, orientation) 1 innerMap
                                            in Map.insert (pos, orientation) innerMap' acc
                                        )
                                        tmpGraph
                                        newEntries
                        in Map.union graph' graph
                    else graph
            )
            Map.empty
            [(row, col) | row <- [0..(numRows - 1)], col <- [0..(numCols - 1)]]

findBestScore :: Graph -> Node -> (Node, Node) -> Int
findBestScore graph startNode (endNode1, endNode2) =
    let pq = MinQueue.fromList [(0, startNode, [startNode])]
        initCosts = Map.fromList [(startNode, 0)]
        dijkstra pq minCost bestPaths costs
            | MinQueue.null pq = minCost
            | otherwise =
                let ((cost, currentNode, path), pq') = MinQueue.deleteFindMin pq
                in if cost > minCost
                        then dijkstra MinQueue.empty minCost bestPaths costs
                        else if currentNode == endNode1 || currentNode == endNode2
                                then if cost < minCost
                                        then dijkstra pq' cost [path] costs
                                        else if cost == minCost
                                                then dijkstra pq' minCost (bestPaths ++ [path]) costs
                                                else dijkstra pq' minCost bestPaths costs
                                else
                                    let (newPQ, newCosts) =
                                           foldl
                                                (\(tempPQ, tempCosts) (neighbor, edgeCost) ->
                                                    let newCost = cost + edgeCost
                                                        neighborCost = Map.findWithDefault (maxBound :: Int) neighbor costs
                                                    in if newCost <= neighborCost
                                                            then (MinQueue.insert (newCost, neighbor, path ++ [neighbor]) tempPQ,
                                                                  Map.insert neighbor newCost tempCosts)
                                                            else (tempPQ, tempCosts)
                                                )
                                                (pq', costs)
                                                (Map.toList $ graph Map.! currentNode)
                                    in dijkstra newPQ minCost bestPaths newCosts
        in dijkstra pq (maxBound :: Int) [] Map.empty

process :: String -> Int
process contents =
    let start = findStart (lines contents)
        end = findEnd (lines contents)
        graph = buildGraph contents
    in findBestScore graph (start, Horizontal) ((end, Horizontal), (end, Vertical))

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            contents <- readFile filename
            let result = process contents
            putStrLn $ "result = " ++ show result
        _ -> usage
