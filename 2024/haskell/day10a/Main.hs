module Main ( main ) where

import Data.Bifunctor ( Bifunctor(bimap) )
import Data.Char ( digitToInt )
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Data.Set ( Set )
import qualified Data.Set as Set
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure )

type Position = (Int, Int)
type Grid = Map (Int, Int) Int

usage :: IO ()
usage = do
  progname <- getProgName
  putStrLn $ "usage: " ++ progname ++ " <file>"
  exitFailure

buildGrid :: String -> Grid
buildGrid contents = Map.fromList [((i, j), digitToInt c) | (i, row) <- zip [0..] (lines contents), (j, c) <- zip [0..] row]

findTrailheads :: Grid -> [Position]
findTrailheads grid = Map.keys (Map.filterWithKey (\k v -> v == 0) grid)

computeTrailScore :: Grid -> Position -> Int
computeTrailScore grid trailhead =
    let dirs = [(-1, 0), (0, 1), (1, 0), (0, -1)]
        walkTrail [] endpoints = Set.size endpoints
        walkTrail (currPath:rest) endpoints =
            let pos = last currPath
                height = grid Map.! pos
            in if height == 9
                then walkTrail rest (Set.insert pos endpoints)
                else
                    let newPaths = map (\x -> currPath ++ [x]) $ 
                                    filter (\x -> (grid Map.! x) == (height + 1)) $
                                    filter (`Map.member` grid) $
                                    [bimap (r +) (c +) pos | (r, c) <- dirs]
                    in walkTrail (rest ++ newPaths) endpoints
    in walkTrail [[trailhead]] Set.empty

process :: String -> Int
process contents =
    let grid = buildGrid contents
        trailheads = findTrailheads grid
    in sum $ map (computeTrailScore grid) trailheads

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      contents <- readFile filename
      let result = process contents
      putStrLn $ "result = " ++ show result
    _ -> usage
