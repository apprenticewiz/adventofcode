module Main ( main ) where

import Control.Applicative
import Data.List
import Data.List.Split
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Data.Maybe
import System.Environment
import System.Exit
import System.IO
import Control.DeepSeq
import System.Clock

type Grid = [String]

type RuleBook = Map Grid Grid

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

parseRule :: String -> (Grid, Grid)
parseRule line =
    case splitOn " => " line of
        [lhs, rhs] -> (splitOn "/" lhs, splitOn "/" rhs)
        _          -> error $ "malformed input: " ++ line

variants :: Grid -> [Grid]
variants g = [ r | f <- flips g, r <- rotations f ]
  where
    flips x = [x, map reverse x]
    rotations x = take 4 (iterate rotate x)
    rotate = reverse . transpose

splitGrid :: Grid -> [Grid]
splitGrid g =
  let n = length g
      size = if even n then 2 else 3
      stripes = chunksOf size g
  in [ [ take size (drop (c * size) row) | row <- blockRows ]
       | blockRows <- stripes
       , c <- [0 .. (n `div` size) - 1] ]

joinBlocks :: [Grid] -> Grid
joinBlocks blocks =
    let n = round (sqrt (fromIntegral (length blocks) :: Double))
        rows = chunksOf n blocks
    in concatMap (foldl1 (zipWith (++))) rows

enhanceBlock :: RuleBook -> Grid -> Grid
enhanceBlock rules block =
  fromJust $ foldr (\v acc -> acc <|> Map.lookup v rules) Nothing (variants block)

enhance :: RuleBook -> Grid -> Grid
enhance rules g = joinBlocks (map (enhanceBlock rules) (splitGrid g))

iterateEnhance :: Int -> RuleBook -> Grid -> Grid
iterateEnhance 0 _ g = g
iterateEnhance n rules g = iterateEnhance (n-1) rules (enhance rules g)

countLit :: Grid -> Int
countLit = length . concatMap (filter (== '#'))

process :: String -> Int
process content =
    let rulesList = map parseRule (lines content)
        rules = Map.fromList [ (v, rhs) | (lhs, rhs) <- rulesList, v <- variants lhs ]
        start = [".#.","..#","###"]
        final = iterateEnhance 5 rules start
    in countLit final


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
