module Main ( main ) where

import Data.List ( foldl', nub )
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

startsWith :: String -> Char -> Bool
startsWith s ch = head s == ch

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

process :: String -> Int
process contents =
    let graph = buildGraph contents            
        triangles = findTriangles graph
    in length $ filter (any (`startsWith` 't') . Set.toList) triangles

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            contents <- readFile filename
            let result = process contents
            putStrLn $ "result = " ++ show result
        _ -> usage
