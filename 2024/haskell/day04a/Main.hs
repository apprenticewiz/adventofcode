module Main ( main ) where

import Data.Bifunctor ( Bifunctor(bimap) )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure )

usage :: IO ()
usage = do
  progname <- getProgName
  putStrLn $ "usage: " ++ progname ++ " <file>"
  exitFailure

process :: String -> Int
process contents =
    let grid = lines contents
        charAt grid row col = (grid !! row) !! col
        maxRow = length grid
        maxCol = length (head grid)
        inBounds row col = row >= 0 && row < maxRow && col >= 0 && col < maxCol
        deltas =
            [
              [(0, 0), (-1, -1), (-2, -2), (-3, -3)],
              [(0, 0), (-1, 0), (-2, 0), (-3, 0)],
              [(0, 0), (-1, 1), (-2, 2), (-3, 3)],
              [(0, 0), (0, -1), (0, -2), (0, -3)],
              [(0, 0), (0, 1), (0, 2), (0, 3)],
              [(0, 0), (1, -1), (2, -2), (3, -3)],
              [(0, 0), (1, 0), (2, 0), (3, 0)],
              [(0, 0), (1, 1), (2, 2), (3, 3)]
            ]
        word = "XMAS"
    in length $ foldl
        (\result startingCoords ->
            let coordsToScan = filter (all (uncurry inBounds))
                    (map (map (bimap (fst startingCoords +) (snd startingCoords +))) deltas)
                hasWord coordsList =
                    let chars = map (uncurry (charAt grid)) coordsList
                    in chars == word
                matches = filter hasWord coordsToScan
            in if not (null matches)
                then result ++ replicate (length matches) startingCoords
                else result
        )
        []
        (filter (\(r, c) -> charAt grid r c == 'X') [(r, c) | r <- [0..(maxRow - 1)], c <- [0..(maxCol - 1)]])

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      contents <- readFile filename
      let result = process contents
      putStrLn $ "result = " ++ show result
    _ -> usage
