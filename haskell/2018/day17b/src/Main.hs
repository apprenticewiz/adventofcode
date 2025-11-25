module Main ( main ) where

import Control.Monad
import Control.Monad.State
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import System.Environment
import System.Exit
import System.IO

type Position = (Int, Int)

data Cell = Clay | Flowing | Settled
            deriving (Eq, Show)

type Grid = Map Position Cell

data World = World { grid :: Grid, minY :: Int, maxY :: Int } deriving (Show)

type Water = State World

parseLine :: String -> [Position]
parseLine line =
    case words (map (\c -> if c `elem` "=,." then ' ' else c) line) of
        ["x", xs, "y", y1s, y2s] -> [ (read xs, y) | y <- [read y1s..read y2s] ]
        ["y", ys, "x", x1s, x2s] -> [ (x, read ys) | x <- [read x1s..read x2s] ]
        _ -> error "Invalid line"

readCell :: Position -> Water (Maybe Cell)
readCell p = gets (Map.lookup p . grid)

writeCell :: Position -> Cell -> Water ()
writeCell p v = modify (\e -> e { grid = Map.insert p v (grid e) })

flowDown :: Position -> Water ()
flowDown (x, y) = do
    my <- gets maxY
    if y > my then
        return ()
    else do
        below <- readCell (x, y+1)
        case below of
            Nothing -> do
                writeCell (x,y) Flowing
                flowDown (x, y+1)
            Just Clay    -> flowSideways (x,y)
            Just Settled -> flowSideways (x,y)
            Just Flowing -> writeCell (x,y) Flowing

flowSideways :: Position -> Water ()
flowSideways (x, y) = do
    writeCell (x,y) Flowing

    (xl, lf) <- scanDir (-1) (x,y)
    (xr, rf) <- scanDir 1    (x,y)

    forM_ [xl..xr] $ \x' -> writeCell (x', y) Flowing

    case (lf, rf) of
        (Nothing, Nothing) -> do
            fillSettled xl xr y
            when (y > 0) $ flowSideways (x, y - 1)
        _ -> do
            forM_ lf $ \p -> flowDown p
            forM_ rf $ \p -> flowDown p

fillSettled :: Int -> Int -> Int -> Water ()
fillSettled xl xr y =
    forM_ [xl..xr] $ \x ->
        writeCell (x,y) Settled

scanDir :: Int -> Position -> Water (Int, Maybe Position)
scanDir dx (x0, y0) = go x0
  where
    go x = do
        let below = (x,     y0 + 1)
        cellBelow <- readCell below
        case cellBelow of
            Nothing      -> return (x, Just below)
            Just Flowing -> return (x, Just below)
            Just Clay    -> checkNext
            Just Settled -> checkNext
      where
        checkNext = do
            nextCell <- readCell (x + dx, y0)
            case nextCell of
                Just Clay -> return (x, Nothing)
                _         -> go (x + dx)

simulate :: Grid -> (Grid, Int, Int)
simulate g =
    let ys = map snd (Map.keys g)
        mn = minimum ys
        mx = maximum ys
        env0 = World g mn mx
        final = execState (flowDown (500, 0)) env0
    in (grid final, mn, mx)

countWater :: Int -> Int -> Grid -> Int
countWater mn mx g =
    length [ () | ((_, y), c) <- Map.toList g
                , y >= mn && y <= mx
                , c == Settled ]

process :: String -> Int
process content =
    let g = concatMap parseLine (lines content)
        grid0 = Map.fromList [(pos, Clay) | pos <- g]
        (finalGrid, mnY, mxY) = simulate grid0
    in countWater mnY mxY finalGrid

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
