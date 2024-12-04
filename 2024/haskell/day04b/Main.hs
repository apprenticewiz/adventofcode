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
    let grid = words contents
        maxRow = length grid
        maxCol = length (head grid)
        leftDelta = [(-1, -1), (0, 0), (1, 1)]
        rightDelta = [(-1, 1), (0, 0), (1, -1)]
        word1 = "MAS"
        word2 = reverse word1
    in length $ foldl
        (\result startingCoords ->
            let leftCoordsToScan = filter (\(r, c) -> r >= 0 && r < maxRow && c >= 0 && c < maxCol)
                   (map (bimap (fst startingCoords +) (snd startingCoords +)) leftDelta)
                rightCoordsToScan = filter (\(r, c) -> r >= 0 && r < maxRow && c >= 0 && c < maxCol)
                   (map (bimap (fst startingCoords +) (snd startingCoords +)) rightDelta)
                hasX leftCoords rightCoords =
                    let leftChars = map (\(r, c) -> (grid !! r) !! c) leftCoords
                        rightChars = map (\(r, c) -> (grid !! r) !! c) rightCoords
                    in (leftChars == word1 || leftChars == word2) && (rightChars == word1 || rightChars == word2)
            in if hasX leftCoordsToScan rightCoordsToScan
                then result ++ [startingCoords]
                else result
        )
        []
        (filter (\(r, c) -> ((grid !! r) !! c) == 'A') [(r, c) | r <- [0..(maxRow - 1)], c <- [0..(maxCol - 1)]])

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      contents <- readFile filename
      let result = process contents
      putStrLn $ "result = " ++ show result
    _ -> usage
