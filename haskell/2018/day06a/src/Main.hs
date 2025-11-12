{- HLINT ignore "Redundant bracket" -}
module Main ( main ) where

import Data.Bifunctor
import Data.List
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set ( Set )
import qualified Data.Set as Set
import System.Environment
import System.Exit
import System.IO

type Position = (Int, Int)

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

bounds :: [Position] -> (Position, Position)
bounds coords =
    let minX = minimum $ map fst coords
        minY = minimum $ map snd coords
        maxX = maximum $ map fst coords
        maxY = maximum $ map snd coords
    in ((minX, minY), (maxX, maxY))

manhattanDistance :: Position -> Position -> Int
manhattanDistance (x1, y1) (x2, y2) = (abs (x2 - x1)) + (abs (y2 - y1))

mapOwners :: [Position] -> (Position, Position) -> Map Position (Maybe Int)
mapOwners points extents =
    let ((minX, minY), (maxX, maxY)) = extents
    in Map.fromList [ ((x, y), o) | x <- [minX..maxX]
                                  , y <- [minY..maxY]
                                  , let distances = map (manhattanDistance (x, y)) points
                                  , let minDist = minimum distances
                                  , let o = if length (elemIndices minDist distances) == 1
                                                then Just (fromJust $ elemIndex minDist distances)
                                                else Nothing
                    ]

findInfinites :: (Position, Position) -> Map Position (Maybe Int) -> Set Int
findInfinites ((minX, minY), (maxX, maxY)) ownerMap =
    Set.fromList [ fromJust o | ((x, y), o) <- Map.toList ownerMap
                              , isJust o
                              , (x == minX || x == maxX) || (y == minY || y == maxY)
                 ]

countAreas :: Map Position (Maybe Int) -> Set Int -> Map Int Int
countAreas ownerMap infinites = foldr (\i -> Map.insertWith (+) i 1) Map.empty
    ((filter (`Set.notMember` infinites) . catMaybes . Map.elems) ownerMap)

process :: String -> Int
process content =
    let points = map parseCoords (lines content)
        extents = bounds points
        ownerMap = mapOwners points extents
        infinites = findInfinites extents ownerMap
        areas = countAreas ownerMap infinites
    in maximum (Map.elems areas)
  where
    parseCoords :: String -> Position
    parseCoords s =
        let (xStr, _:yStr) = span (/= ',') s
        in bimap read read (xStr, yStr)

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
