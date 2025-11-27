module Main ( main ) where

import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Data.Set ( Set )
import qualified Data.Set as Set
import System.Environment
import System.Exit
import System.IO
import Text.Parsec
import Text.Parsec.String

data Regex =
    Seq [Regex]
  | Branch [Regex]
  | Move Char
  deriving (Eq, Show)

type Position = (Int, Int)
type Graph = Map Position (Set Position)

file :: Parser Regex
file = between (char '^') (char '$') regex

regex :: Parser Regex
regex = Seq <$> many term

term :: Parser Regex
term = Move <$> oneOf "NSEW" <|> branch

branch :: Parser Regex
branch = between (char '(') (char ')') $ do
    parts <- sepBy regex (char '|')
    return (Branch parts)

addEdge :: Position -> Position -> Graph -> Graph
addEdge a b g = g2
  where
    g1 = Map.insertWith Set.union a (Set.singleton b) g
    g2 = Map.insertWith Set.union b (Set.singleton a) g1

move :: Char -> Position -> Position
move dir (x, y) =
    case dir of
        'N' -> (x, y - 1)
        'S' -> (x, y + 1)
        'E' -> (x - 1, y)
        'W' -> (x + 1, y)
        _   -> (x, y)

build :: Regex -> Set Position -> Graph -> (Set Position, Graph)
build (Move c) starts g = Set.foldr step (Set.empty, g) starts
  where
    step pos (acc, g') =
        let pos' = move c pos
            g'' = addEdge pos pos' g'
        in (Set.insert pos' acc, g'')
build (Seq []) starts g = (starts, g)
build (Seq (r:rs)) starts g =
    let (mid, g1) = build r starts g
    in build (Seq rs) mid g1
build (Branch rs) starts g = foldl combine (Set.empty, g) rs
  where
    combine (acc, g1) r =
        let (ends, g2) = build r starts g1
        in (Set.union acc ends, g2)

bfs :: Graph -> Position -> Map Position Int
bfs g start = go (Seq.singleton start) (Map.singleton start 0)
  where
    go Seq.Empty dist = dist
    go (x Seq.:<| xs) dist =
        let d = dist Map.! x
            neighbors = Map.findWithDefault Set.empty x g
            (xs', dist') = Set.foldr (visit d) (xs, dist) neighbors
        in go xs' dist'

    visit d n (q, dist) =
        if Map.member n dist
            then (q, dist)
            else (q Seq.|> n, Map.insert n (d + 1) dist)

process :: String -> Int
process content =
    case parse file "" content of
        Left err -> error (show err)
        Right r ->
            let (_, graph) = build r (Set.singleton (0, 0)) Map.empty
                dists = bfs graph (0, 0)
            in maximum (Map.elems dists)

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

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
