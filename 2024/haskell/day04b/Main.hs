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
        inBounds row col = row >= 0 && row < maxRow && col >= 0 && col < maxRow
        leftDelta = [(-1, -1), (0, 0), (1, 1)]
        rightDelta = [(-1, 1), (0, 0), (1, -1)]
        word1 = "MAS"
        word2 = reverse word1
    in length $ foldl
        (\result startingCoords ->
            let leftCoordsToScan = filter (uncurry inBounds)
                   (map (bimap (fst startingCoords +) (snd startingCoords +)) leftDelta)
                rightCoordsToScan = filter (uncurry inBounds)
                   (map (bimap (fst startingCoords +) (snd startingCoords +)) rightDelta)
                hasX leftCoords rightCoords =
                    let leftChars = map (uncurry (charAt grid)) leftCoords
                        rightChars = map (uncurry (charAt grid)) rightCoords
                    in (leftChars == word1 || leftChars == word2) && (rightChars == word1 || rightChars == word2)
            in if hasX leftCoordsToScan rightCoordsToScan
                then result ++ [startingCoords]
                else result
        )
        []
        (filter (\(r, c) -> charAt grid r c == 'A') [(r, c) | r <- [0..(maxRow - 1)], c <- [0..(maxCol - 1)]])

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      contents <- readFile filename
      let result = process contents
      putStrLn $ "result = " ++ show result
    _ -> usage
