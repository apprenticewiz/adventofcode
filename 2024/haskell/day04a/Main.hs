module Main ( main ) where

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
        dirs = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]
        word = "XMAS"
    in length $ foldl
        (\result startingCoords ->
            let coordsToScan = filter (all (\(r, c) -> r >= 1 && r <= maxRow && c >= 1 && c <= maxCol)) $
                    map (\(deltaRow, deltaCol) -> take 4 $ iterate (\(r, c) -> (r + deltaRow, c + deltaCol)) startingCoords) dirs
                hasWord coordsList =
                    let chars = map (grid Map.!) coordsList
                    in chars == word
                matches = filter hasWord coordsToScan
            in if not (null matches)
                then result ++ replicate (length matches) startingCoords
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
