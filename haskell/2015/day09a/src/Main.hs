module Main ( main ) where

import Control.DeepSeq
import Data.Int (Int32)
import Data.List (permutations)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.Clock
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Parsec
import Text.Parsec.String (Parser)

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

type City = String
type Distances = Map.Map (City, City) Int

city :: Parser City
city = many1 letter

line :: Parser (City, City, Int)
line = do
       city1 <- city
       spaces
       _ <- string "to"
       spaces
       city2 <- city
       spaces
       _ <- string "="
       spaces
       distance <- many1 digit
       return (city1, city2, read distance)

file :: Parser [(City, City, Int)]
file = line `endBy1` newline

buildMap :: [(City, City, Int)] -> Distances
buildMap edges = Map.fromList [((a, b), n) | (a, b, n) <- edges ] <>
                 Map.fromList [((b, a), n) | (a, b, n) <- edges ]

dist :: Distances -> City -> City -> Int
dist d a b = d Map.! (a, b)

pathLen :: Distances -> [City] -> Int
pathLen d cs = sum [ dist d a b | (a, b) <- zip cs (tail cs) ]

shortestRoute :: [City] -> Distances -> Int
shortestRoute cities d = minimum [ pathLen d p | p <- permutations cities ]

process :: String -> Int32
process content =
    let edges = case parse file "" content of
                    Left err -> error (show err)
                    Right xs -> xs
        distances = buildMap edges
        cities = Set.toList (Set.fromList [ c | (a, b, _) <- edges, c <- [a, b] ])
    in fromIntegral $ shortestRoute cities distances

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
