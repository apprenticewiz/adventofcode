module Main ( main ) where

import Data.List ( foldl', intercalate, nub, sort )
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Data.Set ( Set )
import qualified Data.Set as Set
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure )

usage :: IO ()
usage = do
    progname <- getProgName
    putStrLn $ "usage: " ++ progname ++ " <file>"
    exitFailure

buildGraph :: String -> Map String [String]
buildGraph contents =
    foldl'
        (\acc line ->
            let (first, _:second) = break (== '-') line
                acc' = Map.insertWith (++) first [second] acc
            in Map.insertWith (++) second [first] acc'
        )
        Map.empty
        (lines contents)

findTriangles :: Map String [String] -> [Set String]
findTriangles graph =
    let vertices = Map.keys graph
    in nub $ foldl'
        (\acc vertex ->
            let otherNodes = graph Map.! vertex
                pairs = [(x, y) | x <- otherNodes, y <- otherNodes, x /= y]
                isTriangle (x, y) = x `elem` (graph Map.! y) && y `elem` (graph Map.! x)
            in acc ++ map (\(x, y) -> Set.fromList [vertex, x, y]) (filter isTriangle pairs)
        )
        []
        vertices

computeHistogram :: [Set String] -> Map String Int
computeHistogram =
    foldl'
        (\acc triangle ->
            foldl' (\acc' elem -> Map.insertWith (+) elem 1 acc') acc (Set.toList triangle)
        )
        Map.empty

findClique :: [Set String] -> Map String Int -> Set String
findClique triangles histogram =
    let maxTriangles = Set.findMax (Set.fromList (Map.elems histogram))
    in foldl'
        (\acc triangle ->
            let s = Set.fromList triangle
            in Set.union acc s
        )
        Set.empty
        (filter (all (\y -> histogram Map.! y == maxTriangles)) (map Set.toList triangles))

process :: String -> String
process contents =
    let graph = buildGraph contents
        triangles = findTriangles graph
        histogram = computeHistogram triangles
        clique = findClique triangles histogram
    in intercalate "," $ sort (Set.toList clique)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            contents <- readFile filename
            let result = process contents
            putStrLn $ "result = " ++ result
        _ -> usage
