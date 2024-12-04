module Main ( main ) where

import Data.Bifunctor ( Bifunctor(bimap) )
import qualified Data.Map as Map
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure )

usage :: IO ()
usage = do
  progname <- getProgName
  putStrLn $ "usage: " ++ progname ++ " <file>"
  exitFailure

process :: String -> Int
process contents =
    let grid =
            Map.fromList $ fst $ foldl
                (\(cells, (row, col)) ch ->
                    case ch of
                        '\n' -> (cells, (row + 1, 1))
                        _ -> (cells ++ [((row, col), ch)], (row, col + 1))
                )
                ([], (1, 1))
                contents
        extents = maximum (Map.keys grid)
        maxRow = fst extents
        maxCol = snd extents
        leftDelta = [(-1, -1), (0, 0), (1, 1)]
        rightDelta = [(-1, 1), (0, 0), (1, -1)]
        word1 = "MAS"
        word2 = reverse word1
    in length $ foldl
        (\result startingCoords ->
            let leftCoordsToScan = filter (\(r, c) -> r >= 1 && r <= maxRow && c >= 1 && c <= maxCol)
                   (map (bimap (fst startingCoords +) (snd startingCoords +)) leftDelta)
                rightCoordsToScan = filter (\(r, c) -> r >= 1 && r <= maxRow && c >= 1 && c <= maxCol)
                   (map (bimap (fst startingCoords +) (snd startingCoords +)) rightDelta)
                hasX leftCoords rightCoords =
                    let leftChars = map (grid Map.!) leftCoords
                        rightChars = map (grid Map.!) rightCoords
                    in (leftChars == word1 || leftChars == word2) && (rightChars == word1 || rightChars == word2)
            in if hasX leftCoordsToScan rightCoordsToScan
                then result ++ [startingCoords]
                else result
        )
        []
        (Map.keys grid)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      contents <- readFile filename
      let result = process contents
      putStrLn $ "result = " ++ show result
    _ -> usage
