module Main ( main ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict ( Map )
import qualified Data.Sequence as Seq
import Data.Sequence ( Seq )
import qualified Data.Set as Set
import Data.Set ( Set )
import System.Environment
import System.Exit
import System.IO
import Text.Parsec
import Text.Parsec.String

type Coordinate = (Int, Int)

data Node = Node { size :: Int, used :: Int } deriving (Show)

type Grid = Map Coordinate Node

type Graph = Map Coordinate [Coordinate]

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <filename> <input string>"
    exitFailure

file :: Parser Grid
file = do
    _ <- manyTill anyChar newline >> manyTill anyChar newline
    rows <- many line <* eof
    return $ Map.fromList rows

line :: Parser (Coordinate, Node)
line = do
    _ <- string "/dev/grid/node-"
    _ <- char 'x'
    x <- read <$> many1 digit
    _ <- char '-'
    _ <- char 'y'
    y <- read <$> many1 digit
    _ <- spaces
    sizeVal <- read <$> many1 digit
    _ <- char 'T' >> spaces
    usedVal <- read <$> many1 digit
    _ <- char 'T' >> manyTill anyChar newline
    return ((x, y), Node sizeVal usedVal)

buildGraph :: Grid -> Graph
buildGraph grid =
    Map.fromList [ ((x, y),
                     [ (nx, ny) | (dx, dy) <- [(-1, 0), (1, 0), (0, -1), (0, 1)]
                                , let (nx, ny) = (x + dx, y + dy)
                                , (nx, ny) `elem` (Map.keys grid)
                                , used (grid Map.! (x, y)) < 100
                                , used (grid Map.! (nx, ny)) < 100
                     ])
                 | (x, y) <- Map.keys grid
                 ]

emptyCell :: Grid -> Coordinate
emptyCell grid = head [ (x, y) | ((x, y), n) <- Map.toList grid
                               , used n == 0
                      ]

process :: String -> Int
process content =
    case parse file "" content of
        Left err -> error (show err)
        Right grid ->
            let graph = buildGraph grid
                startNode = emptyCell grid
                maxX = maximum [ x | (x, _) <- Map.keys grid ]
                endNode = (maxX - 1, 0)
                shortestPath =
                    bfs (Seq.singleton startNode) (Set.singleton startNode) (Map.singleton startNode Nothing) graph endNode
            in length shortestPath + (5 * (maxX - 1))
    where
        bfs :: Seq Coordinate -> Set Coordinate -> Map Coordinate (Maybe Coordinate) -> Graph -> Coordinate -> [Coordinate]
        bfs Seq.Empty _ _ _ _ = error "no path found"
        bfs (current Seq.:<| rest) visited parentMap graph endNode
            | current == endNode = reconstructPath parentMap endNode
            | otherwise =
                let neighbors = Map.findWithDefault [] current graph
                    newNeighbors = filter (`Set.notMember` visited) neighbors
                    visited' = foldr Set.insert visited newNeighbors
                    parentMap' = foldr (\n -> Map.insert n (Just current)) parentMap newNeighbors
                    queue' = rest <> Seq.fromList newNeighbors
                in bfs queue' visited' parentMap' graph endNode

        reconstructPath :: Map Coordinate (Maybe Coordinate) -> Coordinate -> [Coordinate]
        reconstructPath parentMap node =
            case Map.lookup node parentMap of
                Just Nothing -> [node]
                Just (Just parent) -> reconstructPath parentMap parent ++ [node]
                Nothing -> []

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
