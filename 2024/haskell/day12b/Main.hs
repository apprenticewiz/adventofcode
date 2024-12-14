module Main ( main ) where

import Data.Array ( Array, (!), bounds, indices, listArray )
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Data.Set ( Set )
import qualified Data.Set as Set
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure )

type Grid = Array (Int, Int) Char

type Position = (Int, Int)

usage :: IO ()
usage = do
    progname <- getProgName
    putStrLn $ "usage: " ++ progname ++ " <file>"
    exitFailure

buildGrid :: String -> Grid
buildGrid contents =
    let rows = lines contents
        numRows = length rows
        numCols = length (head rows)
    in listArray ((0, 0), (numRows - 1, numCols - 1)) (concat rows)

inBounds :: Position -> Grid -> Bool
inBounds pos@(row, col) grid =
    let ((minRow, minCol), (maxRow, maxCol)) = bounds grid
    in row >= minRow && row <= maxRow && col >= minCol && col <= maxCol

neighbors :: Position -> [Position]
neighbors pos@(row, col) = [(row + dr, col + dc) | (dr, dc) <- [(-1, 0), (0, 1), (1, 0), (0, -1)]]

floodFill :: Grid -> Position -> Char -> Set Position -> Set Position
floodFill grid pos@(row, col) ch visited
    | not (inBounds pos grid) || grid ! pos /= ch || Set.member pos visited = visited
    | otherwise = foldl (\acc neighbor -> floodFill grid neighbor ch acc) newVisited (neighbors pos)
    where
        newVisited = Set.insert (row, col) visited

findComponents :: Grid -> [(Char, Set Position)]
findComponents grid = findComponents' (indices grid) Set.empty []
    where findComponents' :: [Position] -> Set Position -> [(Char, Set Position)] -> [(Char, Set Position)]
          findComponents' [] _ components = components
          findComponents' (pos:positions) visited components
              | Set.member pos visited = findComponents' positions visited components
              | otherwise =
                    let ch = grid ! pos
                        component = floodFill grid pos ch Set.empty
                    in findComponents' positions (Set.union visited component) ((ch, component):components)

calcAreaAndSides :: Grid -> Set Position -> (Int, Int)
calcAreaAndSides grid positions = (area, sides)
    where
        area = Set.size positions
        sides = countCorners (Set.toList positions)
        countCorners positionList = sum $ map countCornersAt positionList
            where
                isBoundary (nr, nc) (row, col) = not (inBounds (nr, nc) grid) || grid ! (nr, nc) /= grid ! (row, col)
                getChar (row, col) = if inBounds (row, col) grid then grid ! (row, col) else '*'
                countCornersAt (row, col) =
                    let ch = getChar (row, col)
                        nw = getChar (row - 1, col - 1)
                        n = getChar (row - 1, col)
                        ne = getChar (row - 1, col + 1)
                        e = getChar (row, col + 1)
                        se = getChar (row + 1, col + 1)
                        s = getChar (row + 1, col)
                        sw = getChar (row + 1, col - 1)
                        w = getChar (row, col - 1)
                        corners = [nw == ch && n == ch && w /= ch,
                                   n /= ch && w /= ch,
                                   ne == ch && n == ch && e /= ch,
                                   n /= ch && e /= ch,
                                   se == ch && s == ch && e /= ch,
                                   s /= ch && e /= ch,
                                   sw == ch && s == ch && w /= ch,
                                   s /= ch && w /= ch]
                    in length $ filter (== True) corners
                edge =
                    foldl
                        (\acc pos ->
                            let posNeighbors = neighbors pos
                                isEdge = any (`isBoundary` pos) posNeighbors
                            in if isEdge
                                then Set.union acc (Set.singleton pos)
                                else acc
                        )
                        Set.empty
                        positionList

computePrices :: Grid -> [(Char, Set Position)] -> Map Char Int
computePrices grid = foldl
        (\acc component@(ch, positions) ->
            let (area, perimeter) = calcAreaAndSides grid positions
            in Map.insertWith (+) ch (area * perimeter) acc
        )
        Map.empty

process :: String -> Int
process contents =
    let grid = buildGrid contents
        components = findComponents grid
        prices = computePrices grid components
    in sum $ Map.elems prices

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            contents <- readFile filename
            let result = process contents
            putStrLn $ "result = " ++ show result
        _ -> usage
