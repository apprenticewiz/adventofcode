module Main ( main ) where

import Data.Bifunctor
import System.Environment
import System.Exit
import System.IO

type Position = (Int, Int)

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

bounds :: [Position] -> (Position, Position)
bounds coords =
    let minX = minimum $ map fst coords
        minY = minimum $ map snd coords
        maxX = maximum $ map fst coords
        maxY = maximum $ map snd coords
    in ((minX, minY), (maxX, maxY))

manhattanDistance :: Position -> Position -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

safeRegionSize :: [Position] -> Int -> Int
safeRegionSize points threshold =
    let ((minX, minY), (maxX, maxY)) = bounds points
        margin = 100
        region =
            [ (x, y)
            | x <- [minX - margin .. maxX + margin]
            , y <- [minY - margin .. maxY + margin]
            ]
        totalDistance (x, y) = sum [ manhattanDistance (x, y) p | p <- points ]
    in length [ () | pos <- region, totalDistance pos < threshold ]

process :: String -> Int
process content =
    let points = map parseCoords (lines content)
    in safeRegionSize points 10000
  where
    parseCoords :: String -> Position
    parseCoords s =
        let (xStr, _:yStr) = span (/= ',') s
        in bimap read read (xStr, yStr)

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
