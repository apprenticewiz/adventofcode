module Main ( main ) where

import Data.Array ( Array )
import qualified Data.Array as Array
import Data.Bifunctor ( bimap )
import Data.Set ( Set )
import qualified Data.Set as Set
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure )

type Position = (Int, Int)

data Lab = Lab { start :: Position
               , direction :: Int
               , grid :: Array (Int, Int) Char
               , numRows :: Int
               , numCols :: Int
               }
               deriving (Eq, Show)

usage :: IO ()
usage = do
  progname <- getProgName
  putStrLn $ "usage: " ++ progname ++ " <file>"
  exitFailure

parse :: String -> Lab
parse contents =
    let rows = lines contents
        numRows = length rows
        numCols = length (head rows)
        start = head $ [(r, c) | r <- [0..(numRows - 1)], c <- [0..(numCols - 1)], rows !! r !! c == '^']
        elems = [((i, j), c) | i <- [0..numRows - 1], j <- [0..numCols - 1], let c = (rows !! i) !! j]
        grid = Array.array ((0, 0), (numRows - 1, numCols - 1)) elems
    in Lab { start = start, direction = 0, grid = grid, numRows = numRows, numCols = numCols }

directions :: [(Int, Int)]
directions = [(-1, 0), (0, 1), (1, 0), (0, -1)]

inBounds :: Lab -> Position -> Bool
inBounds lab pos@(r, c) = r >= 0 && r < numRows lab && c >= 0 && c < numCols lab

guardPositions :: Lab -> [(Position, Int)]
guardPositions lab =
    let pos = start lab
        dir = direction lab
        guardPositions' pos dir positions =
            let positions' = positions ++ [(pos, dir)]
                nextPos = bimap (fst pos +) (snd pos +) (directions !! dir)
            in if not (inBounds lab nextPos)
                then positions'
                else let dir' = (if grid lab Array.! nextPos == '#'
                                    then (dir + 1) `mod` 4
                                    else dir)
                         pos' = bimap (fst pos +) (snd pos +) (directions !! dir')
                     in guardPositions' pos' dir' positions'
    in guardPositions' pos dir []

hasLoop :: Lab -> Bool
hasLoop !lab = hasLoop' Set.empty (start lab) (direction lab) False
    where
        hasLoop' positions pos dir turn =
            Set.member (pos, dir) positions || (let positions' = Set.insert (pos, dir) positions
                                                    nextPos = bimap (fst pos +) (snd pos +) (directions !! dir)
                                                in (inBounds lab nextPos && (if grid lab Array.! nextPos == '#'
                                                       then hasLoop' positions' pos ((dir + 1) `mod` 4) True
                                                       else hasLoop' positions' nextPos dir False)))

numObstructions :: Lab -> Int
numObstructions lab =
    let route = Set.fromList $ map fst $ guardPositions lab
        loopPos = foldl
                    (\loopPos' pos ->
                        let grid' = grid lab Array.// [(pos, '#')]
                            lab' = Lab { start = start lab
                                       , direction = direction lab
                                       , grid = grid'
                                       , numRows = numRows lab
                                       , numCols = numCols lab
                                       }
                        in if hasLoop lab'
                            then Set.insert pos loopPos'
                            else loopPos'
                    )
                    Set.empty
                    route
    in Set.size loopPos

process :: String -> Int
process contents = numObstructions (parse contents)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      contents <- readFile filename
      let result = process contents
      putStrLn ("result = " ++ show result)
    _ -> usage
