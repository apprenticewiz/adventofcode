module Main ( main ) where

import Data.Array (Array, array, assocs, bounds, (!))
import Data.Char (isDigit)
import qualified Data.Map.Strict as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure )
import System.IO ( hPutStrLn, stderr )
import Control.DeepSeq
import System.Clock

type Coordinate = (Int, Int)

type Grid = Array Coordinate Char

type Graph = Map (Char, Char) Int

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

buildGrid :: String -> Grid
buildGrid content =
    let ls = lines content
        h = length ls
        w = length (head ls)
    in array ((0, 0), (h - 1, w - 1)) [((i, j), (ls !! i) !! j) | i <- [0 .. h - 1], j <- [0 .. w - 1]]

buildGraph :: Grid -> Map Char (Int, Int) -> Graph
buildGraph grid numberLocs =
    foldr insertEdges Map.empty [(c1, c2) | c1 <- Map.keys numberLocs, c2 <- Map.keys numberLocs, c1 < c2]
    where
        insertEdges (c1, c2) g =
            let dist = bfs grid (numberLocs Map.! c1) (numberLocs Map.! c2)
            in Map.insert (c1, c2) dist $ Map.insert (c2, c1) dist g

        bfs :: Grid -> Coordinate -> Coordinate -> Int
        bfs grid start goal = go Set.empty [(start, 0)]
            where
                go _ [] = error "No path found"
                go visited ((pos, dist):queue)
                    | pos == goal = dist
                    | Set.member pos visited = go visited queue
                    | otherwise =
                        let visited' = Set.insert pos visited
                            neighbors = filter isValidNeighbor (adjacentPositions pos)
                            newQueue = queue ++ [(n, dist + 1) | n <- neighbors]
                        in go visited' newQueue

                isValidNeighbor (i, j) =
                    inBounds (i, j) && (grid ! (i, j)) /= '#'

                inBounds (i, j) =
                    let ((minI, minJ), (maxI, maxJ)) = bounds grid
                    in i >= minI && i <= maxI && j >= minJ && j <= maxJ

                adjacentPositions (i, j) =
                    [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)]

findMinPath :: Graph -> Char -> Char -> Set Char -> Int
findMinPath graph origin current visited
    | Set.size visited == Set.size allNodes =
        case Map.lookup (current, origin) graph of
            Just d  -> d
            Nothing -> error "No path back to origin"
    | otherwise =
        minimum
            [ dist + findMinPath graph origin next (Set.insert next visited)
            | ((c1, c2), dist) <- Map.toList graph
            , c1 == current
            , let next = c2
            , not (Set.member next visited)
            ]
  where
    allNodes = Set.fromList [ c | (a, b) <- Map.keys graph, c <- [a, b] ]

process :: String -> Int
process content =
    let grid = buildGrid content
        numberLocs = Map.fromList
            [ (c, (i, j))
            | ((i, j), c) <- assocs grid
            , isDigit c
            ]
        graph = buildGraph grid numberLocs
    in findMinPath graph '0' '0' (Set.singleton '0')


showTime :: TimeSpec -> String
showTime elapsed =
    let ns = fromIntegral (toNanoSecs elapsed) :: Double
    in if ns < 1000
       then show ns ++ " ns"
       else if ns < 1000000
       then show (ns / 1000.0) ++ " μs"
       else if ns < 1000000000
            then show (ns / 1000000.0) ++ " ms"
            else show (ns / 1000000000.0) ++ " s"
main :: IO ()
main = do
    args <- getArgs
    progname <- getProgName
    case args of
        [filename] -> do
            start <- getTime Monotonic
            content <- readFile filename
            let result = process content
            result `deepseq` return ()
            end <- getTime Monotonic
            let elapsed = diffTimeSpec start end
            putStrLn $ "result = " ++ show result
            putStrLn $ "elapsed time: " ++ showTime elapsed
        _ -> usage progname
