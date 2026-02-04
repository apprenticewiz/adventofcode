module Main ( main ) where

import Control.DeepSeq
import Data.Array
import qualified Data.PSQueue as PSQ
import qualified Data.PSQueue as PSQueue
import qualified Data.Set as Set
import System.Clock
import System.Environment
import System.Exit
import System.IO

data Tool = Torch | Climbing | Neither
            deriving (Eq, Ord, Show, Enum, Bounded)

type SearchState = (Int, Int, Tool)

parse :: String -> (Int, (Int, Int))
parse content =
    case lines content of
        [depthLine, targetLine] ->
            let depthStr = last $ words depthLine
                targetCoordsStr = last $ words targetLine
                (t1Str, rest) = span (/= ',') targetCoordsStr
                t2Str = case rest of
                    (_:s) -> s
                    [] -> error "malformed target coordinates"
            in (read depthStr, (read t1Str, read t2Str))
        _ -> error "malformed input"

process :: String -> Int
process content =
    let (depth, target@(targetX, targetY)) = parse content
        maxX = targetX + 50
        maxY = targetY + 50
        erosionLevels = makeErosionLevels depth target (maxX, maxY)
    in dijkstra erosionLevels target

makeErosionLevels :: Int -> (Int, Int) -> (Int, Int) -> Array (Int, Int) Int
makeErosionLevels depth (targetX, targetY) maxBounds@(maxX, maxY) = erosionLevels
  where
    erosionLevels = array ((0, 0), maxBounds) [((x, y), erosionLevel x y) | x <- [0..maxX], y <- [0..maxY]]
    
    erosionLevel x y = (geoIndex x y + depth) `mod` 20183
    
    geoIndex x y
      | x == 0 && y == 0 = 0
      | x == targetX && y == targetY = 0
      | y == 0 = x * 16807
      | x == 0 = y * 48271
      | otherwise = (erosionLevels ! (x - 1, y)) * (erosionLevels ! (x, y - 1))

regionType :: Int -> Int
regionType erosion = erosion `mod` 3

allowed :: Int -> [Tool]
allowed r = case r of
    0 -> [Climbing, Torch]
    1 -> [Climbing, Neither]
    2 -> [Torch, Neither]
    _ -> error "invalid region type"

switchCost :: Int
switchCost = 7

moveCost :: Int
moveCost = 1

neighbors :: Array (Int, Int) Int -> SearchState -> [ (SearchState, Int) ]
neighbors erosion (x,y,tool) =
    let r = regionType (erosion ! (x,y))
        validTools = allowed r
        switchMoves =
            [ ((x,y,t), switchCost)
            | t <- validTools, t /= tool ]
        nbrMoves =
            [ ((nx,ny,tool), moveCost)
            | (nx,ny) <- [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
            , inRange (bounds erosion) (nx,ny)
            , tool `elem` allowed (regionType (erosion ! (nx,ny)))
            ]
    in switchMoves ++ nbrMoves

dijkstra :: Array (Int, Int) Int -> (Int, Int) -> Int
dijkstra erosion (tx, ty) = go initialPQ Set.empty
  where
    start = (0, 0, Torch)
    initialPQ = PSQueue.singleton start 0

    go pq visited =
        case PSQueue.minView pq of
            Nothing -> error "no path found"
            Just (state PSQ.:-> dist, pq')
                | Set.member state visited -> go pq' visited
                | state == (tx, ty, Torch) -> dist
                | otherwise ->
                    let nbrs = neighbors erosion state
                        pq'' = foldl' (\acc (s', d') ->
                                            if Set.member s' visited
                                                then acc
                                                else PSQueue.insertWith min s' (dist + d') acc) pq' nbrs
                    in go pq'' (Set.insert state visited)

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
