module Main ( main ) where

import Control.DeepSeq
import qualified Data.IntMap.Strict as IntMap
import Data.List
import Data.Ord
import System.Clock
import System.Environment
import System.Exit
import System.IO

type Position = (Int, Int)

process :: String -> Int
process content =
    let reds = map parsePoint (lines content)
        edges = zip reds (tail reds ++ [head reds])
        horzList = collectHorz edges
        horzMap = IntMap.map mergeSegments (IntMap.fromListWith (++) horzList)
        minY = minimum (map snd reds)
        maxY = maximum (map snd reds)
        verticalEdgeMap = buildVerticalEdgeMap edges minY maxY
        validXRangesMap = IntMap.fromList 
            [ (y, let horzSegs = IntMap.findWithDefault [] y horzMap
                      crossings = IntMap.findWithDefault [] y verticalEdgeMap
                      interiorRanges = pairUp crossings
                      allRanges = horzSegs ++ interiorRanges
                  in mergeSegments allRanges)
            | y <- [minY..maxY]
            ]
        validRectangle (x1,y1) (x2,y2) =
            let xlo = min x1 x2
                xhi = max x1 x2
                ylo = min y1 y2
                yhi = max y1 y2
                validForY y =
                    case IntMap.lookup y validXRangesMap of
                      Nothing -> False
                      Just ranges -> any (\(a, b) -> a <= xlo && xhi <= b) ranges
                cornersValid = validForY ylo && validForY yhi
            in cornersValid && all validForY [ylo..yhi]
        pairUp [] = []
        pairUp [_] = []
        pairUp (a:b:rest) = (a, b) : pairUp rest
        candidates = [ (area, p1, p2) 
                     | (i,p1@(x1,y1)) <- zip [0..] reds
                     , p2@(x2,y2) <- drop (i+1) reds
                     , x1 /= x2
                     , y1 /= y2
                     , let width = abs (x1-x2) + 1
                     , let height = abs (y1-y2) + 1
                     , let area = width * height
                     ]
        best = case sortBy (comparing (\(a,_,_) -> Down a)) candidates of
                 [] -> 0
                 sorted -> case find (\(_,p1,p2) -> validRectangle p1 p2) sorted of
                             Just (area, _, _) -> area
                             Nothing -> 0
    in best

  where
    parsePoint :: String -> Position
    parsePoint line =
        let (xs,_:ys) = span (/= ',') line
        in (read xs, read ys)

    collectHorz :: [(Position,Position)] -> [(Int,[(Int,Int)])]
    collectHorz = concatMap go
      where
        go ((x1,y1),(x2,y2))
          | y1==y2   = [(y1, [(min x1 x2, max x1 x2)])]
          | otherwise= []

    buildVerticalEdgeMap :: [(Position,Position)] -> Int -> Int -> IntMap.IntMap [Int]
    buildVerticalEdgeMap edges minY maxY =
        let addEdge m ((x1,y1),(x2,y2))
              | x1 /= x2 = m
              | otherwise =
                  let ylo = min y1 y2
                      yhi = max y1 y2
                      -- Only add to Y values in the range [ylo, yhi)
                      inRange = [max minY ylo .. min maxY (yhi-1)]
                  in foldl' (\acc y -> IntMap.insertWith (++) y [x1] acc) m inRange
            raw = foldl' addEdge IntMap.empty edges
        in IntMap.map sort raw

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

mergeSegments :: [(Int,Int)] -> [(Int,Int)]
mergeSegments segs =
    let s = sortOn fst segs
        go [] acc = reverse acc
        go ((a,b):xs) [] = go xs [(a,b)]
        go ((a,b):xs) ((c,d):rest)
          | a <= d + 1 = go xs ((c, max d b):rest)
          | otherwise  = go xs ((a,b):(c,d):rest)
    in go s []

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
