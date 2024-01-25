module Main ( main ) where

import Data.Char
import qualified Data.Map as Map
import Data.Maybe
import System.Environment
import System.Exit
import System.IO

data Position = Position { r :: Int, c :: Int }
  deriving (Eq, Ord, Show)

buildNumbers :: String -> Map.Map Position String
buildNumbers contents =
  let (_, numberLocs) = foldl
                          (\(row, numberLocs) line ->
                              let checkLine (col, scanningNumber, number, currentPos, numLocs) ch =
                                    if scanningNumber
                                      then
                                        if (isDigit ch)
                                          then
                                            if (col + 1) == (length line)
                                              then (col + 1, False, "", Position (-1) (-1), Map.insert currentPos (number ++ [ch]) numLocs)
                                              else (col + 1, True, number ++ [ch], currentPos, numLocs)
                                          else (col + 1, False, "", Position (-1) (-1), Map.insert currentPos number numLocs)
                                      else
                                        if (isDigit ch)
                                          then (col + 1, True, [ch], Position row col, numLocs)                                    
                                          else (col + 1, False, number, currentPos, numLocs)
                                  (_, _, _, _, numberLocs') = foldl checkLine (0, False, "", Position (-1) (-1), numberLocs) line
                              in (row + 1, numberLocs')
                          )
                          (0, Map.empty)
                          (lines contents)
  in numberLocs

buildGears :: String -> Map.Map Position Char
buildGears contents =
  let (_, gearLocs) = foldl
                        (\(row, gearLocs) line ->
                            let checkLine (col, grLocs) ch =
                                  if ch == '*'
                                    then (col + 1, Map.insert (Position row col) ch grLocs)
                                    else (col + 1, grLocs)
                                (_, gearLocs') = foldl checkLine (0, gearLocs) line
                            in (row + 1, gearLocs')
                        )
                        (0, Map.empty)
                        (lines contents)
  in gearLocs

checkGears :: Map.Map Position String -> Map.Map Position Char -> Int
checkGears numberLocs gearLocs =
  foldl
    (\result gearLoc ->
      let checkAdjacents adjacentsList numberLoc =
            let number = fromJust $ Map.lookup numberLoc numberLocs
                numberRow = r numberLoc
                numberColFirst = c numberLoc
                numberColLast = (c numberLoc) + (length number) - 1
                neighbors = [ Position (-1) (-1), Position (-1) 0, Position (-1) 1,
                              Position 0 (-1), Position 0 1,
                              Position 1 (-1), Position 1 0, Position 1 1 ]
                checkNeighbor found neighbor =
                  let adjacentPos = Position ((r gearLoc) + (r neighbor)) ((c gearLoc) + (c neighbor))
                  in found || (((r adjacentPos) == numberRow) &&
                                ((c adjacentPos) >= numberColFirst) &&
                                ((c adjacentPos) <= numberColLast))
                foundAdjacent = foldl checkNeighbor False neighbors
            in
              if foundAdjacent
                then (adjacentsList ++ [(read number)])
                else adjacentsList
          adjacents = foldl checkAdjacents [] (Map.keys numberLocs)
      in
        if (length adjacents) == 2
          then (result + (product adjacents))
          else result
    )
    0
    (Map.keys gearLocs)

process :: String -> Int
process contents =
  let numberLocs = buildNumbers contents
      gearLocs = buildGears contents
  in checkGears numberLocs gearLocs

usage :: IO a
usage = do
  progname <- getProgName
  putStrLn $ "usage: " ++ progname ++ " <file>"
  exitWith (ExitFailure 1)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> usage
    [filename] -> do
      contents <- readFile filename
      let result = process contents
      putStrLn $ "result = " ++ show result
    _ -> usage
