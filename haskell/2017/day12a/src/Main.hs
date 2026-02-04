module Main ( main ) where

import Control.DeepSeq
import Data.Map.Strict ( Map )
import Data.Sequence ( Seq )
import Data.Set ( Set )
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import System.Clock
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
        Right graph -> bfs (Seq.singleton 0) Set.empty graph
        where
            bfs :: Seq Int -> Set Int -> Graph -> Int
            bfs Seq.Empty visited _ = Set.size visited
            bfs (n Seq.:<| rest) visited graph =
                let visited' = Set.insert n visited
                    neighbors = graph Map.! n
                    nextSteps = filter (`Set.notMember` visited) neighbors
                in bfs (rest Seq.>< Seq.fromList nextSteps) visited' graph

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
