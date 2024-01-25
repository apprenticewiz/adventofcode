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

buildParts :: String -> Map.Map Position Char
buildParts contents =
  let (_, partLocs) = foldl
                        (\(row, partLocs) line ->
                            let checkLine (col, prtLocs) ch =
                                  if (not (isDigit ch)) && ch /= '.'
                                    then (col + 1, Map.insert (Position row col) ch prtLocs)
                                    else (col + 1, prtLocs)
                                (_, partLocs') = foldl checkLine (0, partLocs) line
                            in (row + 1, partLocs')
                        )
                        (0, Map.empty)
                        (lines contents)
  in partLocs

checkParts :: Map.Map Position String -> Map.Map Position Char -> Int
checkParts numberLocs partLocs =
  foldl
    (\result numberLoc ->
        let number = fromJust $ Map.lookup numberLoc numberLocs
            numberRow = (r numberLoc)
            numberColFirst = (c numberLoc)
            numberColLast = (c numberLoc) + (length number) - 1
            checkAdjacents found numberCol =
              let checkNeighbor foundAdjacent neighbor =
                    let adjacentPos = Position (numberRow + (r neighbor)) (numberCol + (c neighbor))
                    in foundAdjacent || (Map.member adjacentPos partLocs)
                  neighbors = [ Position (-1) (-1), Position (-1) 0, Position (-1) 1,
                                Position 0 (-1), Position 0 1,
                                Position 1 (-1), Position 1 0, Position 1 1 ]
              in found || (foldl checkNeighbor found neighbors)
            foundAdjacent = foldl checkAdjacents False [numberColFirst..numberColLast]
        in
          if foundAdjacent
            then (result + (read number))
            else result
    )
    0
    (Map.keys numberLocs)

process :: String -> Int
process contents =
  let numberLocs = buildNumbers contents
      partLocs = buildParts contents
  in checkParts numberLocs partLocs

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
