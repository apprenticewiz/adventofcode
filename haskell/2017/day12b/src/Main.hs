module Main ( main ) where

import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Data.Sequence ( Seq )
import qualified Data.Sequence as Seq
import Data.Set ( Set )
import qualified Data.Set as Set
import System.Environment
import System.Exit
import System.IO
import Text.Parsec hiding ( State )
import Text.Parsec.String

type Graph = Map Int [Int]

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

file :: Parser Graph
file = do
    pairs <- line `sepEndBy1` newline <* eof
    return (Map.fromList pairs)

line :: Parser (Int, [Int])
line = do
    from <- many1 digit
    _ <- string " <-> "
    to <- many1 digit `sepBy` string ", "
    return (read from, map read to)

process :: String -> Int
process content =
    case parse file "" content of
        Left err -> error (show err)
        Right graph -> length $ foldr
            (\n sets ->
                if all (Set.notMember n) sets
                    then
                        let set = bfs (Seq.singleton n) Set.empty graph
                        in (set:sets)
                    else sets
            ) [] (Map.keys graph)
        where
            bfs :: Seq Int -> Set Int -> Graph -> Set Int
            bfs Seq.Empty visited _ = visited
            bfs (n Seq.:<| rest) visited graph =
                let visited' = Set.insert n visited
                    neighbors = graph Map.! n
                    nextSteps = filter (`Set.notMember` visited) neighbors
                in bfs (rest Seq.>< Seq.fromList nextSteps) visited' graph

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
