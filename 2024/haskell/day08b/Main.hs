module Main ( main ) where

import Data.List ( tails )
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Set ( Set )
import qualified Data.Set as Set
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure )

import Debug.Trace

usage :: IO ()
usage = do
  progname <- getProgName
  putStrLn $ "usage: " ++ progname ++ " <file>"
  exitFailure

process :: String -> Int
process contents =
    let grid = lines contents
        numRows = length grid
        numCols = length (head grid)
        inBounds (r, c) = r >= 0 && r < numRows && c >= 0 && c < numCols
        charAt g (r, c) = (g !! r) !! c
        antennaMap =
            foldl
                (\m (r, c) ->
                    let ch = charAt grid (r, c)
                    in if ch /= '.'
                        then Map.insertWith (++) ch [(r, c)] m
                        else m
                )
                Map.empty
                [(r, c) | r <- [0..(numRows - 1)], c <- [0..(numCols - 1)]]
        combinations xs = [(x, y) | (x:ys) <- tails xs, y <- ys]
        antinodes =
            foldl
                (\antinodeSet freq ->
                    let antennas = antennaMap Map.! freq
                        antinodesOfFreq =
                            foldl
                                (\acc ((r1, c1), (r2, c2)) ->
                                    let dr = r1 - r2
                                        dc = c1 - c2
                                        pairPlusNodes = takeWhile inBounds [(r1 + (n * dr), c1 + (n * dc)) | n <- [0..]]
                                        pairMinusNodes = takeWhile inBounds [(r1 - (n * dr), c1 - (n * dc)) | n <- [0..]]
                                        pairNodes = Set.union (Set.fromList pairPlusNodes) (Set.fromList pairMinusNodes)
                                    in Set.union acc pairNodes
                                )
                                Set.empty
                                (combinations antennas)
                    in Set.union antinodeSet antinodesOfFreq
                )
                Set.empty
                (Map.keys antennaMap)
    in Set.size antinodes

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      contents <- readFile filename
      let result = process contents
      putStrLn $ "result = " ++ show result
    _ -> usage
