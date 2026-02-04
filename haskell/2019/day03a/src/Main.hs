module Main ( main ) where

import Control.DeepSeq
import Data.Maybe ( catMaybes )
import System.Clock
import System.Environment
import System.Exit
import System.IO

type Point = (Int, Int)
type Segment = (Point, Point)

process :: String -> Int
process content =
    let ls = lines content
    in case ls of
         (wire1:wire2:_) ->
            let segments1 = parseSegments wire1
                segments2 = parseSegments wire2
                intersections = catMaybes [ is | (p1, p2) <- segments1
                                               , (p3, p4) <- segments2
                                               , let is = intersection (p1, p2) (p3, p4)
                                          ]
            in minimum [ manhattanDistance (0, 0) is | is <- intersections ]
         _ -> error "malformed input"
    where
        parseSegments :: String -> [Segment]
        parseSegments wire =
            let moves = words . map (\c -> if c == ',' then ' ' else c) $ wire
                (_, finalSegments) = foldl' (\(pos@(px, py), segments) move ->
                                                let pos' = case move of
                                                        'U':dist -> (px, py + read dist)
                                                        'D':dist -> (px, py - read dist)
                                                        'L':dist -> (px - read dist, py)
                                                        'R':dist -> (px + read dist, py)
                                                        _ -> error "malformed input"
                                                    segment = (pos, pos')
                                                    segments' = segments ++ [segment]
                                                in (pos', segments')) ((0, 0), []) moves
            in finalSegments

        intersection :: Segment -> Segment -> Maybe Point
        intersection ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4))
            | x1 == x2 && y3 == y4 =
                let x = x1
                    y = y3
                    minY1 = min y1 y2
                    maxY1 = max y1 y2
                    minX2 = min x3 x4
                    maxX2 = max x3 x4
                in if y >= minY1 && y <= maxY1 && x >= minX2 && x <= maxX2
                   then Just (x, y)
                   else Nothing
            | y1 == y2 && x3 == x4 =
                let x = x3
                    y = y1
                    minX1 = min x1 x2
                    maxX1 = max x1 x2
                    minY2 = min y3 y4
                    maxY2 = max y3 y4
                in if x >= minX1 && x <= maxX1 && y >= minY2 && y <= maxY2
                   then Just (x, y)
                   else Nothing
            | otherwise = Nothing

        manhattanDistance :: Point -> Point -> Int
        manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

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
