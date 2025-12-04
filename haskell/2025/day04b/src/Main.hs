module Main ( main ) where

import Data.Array.Unboxed
import System.Environment
import System.Exit
import System.IO

type Grid = UArray (Int, Int) Char

process :: String -> Int
process content =
    let ls = lines content
        rows = length ls
        cols = length (ls !! 0)
        grid = array ((0, 0), (rows - 1, cols - 1)) [ ((r, c), ls !! r !! c) | r <- [0 .. rows - 1], c <- [0 .. cols - 1] ] :: Grid
    in go grid 0
  where
    go :: Grid -> Int -> Int
    go grid removed =
        let neighbors (r, c) =
                [ (r + dr, c + dc) | (dr, dc) <- [ (-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1) ]
                                   , (bounds grid) `inRange` (r + dr, c + dc)
                ]
            rolls = filter (\(r, c) -> grid ! (r, c) == '@') (indices grid)
            accessible = [ roll | roll <- rolls
                                , let neighborRolls = [ neighbor | neighbor <- neighbors roll, grid ! neighbor == '@' ]
                                , length neighborRolls < 4
                         ]
        in if null accessible
               then removed
               else let removed' = removed + length accessible
                        grid' = grid // [ ((r, c), '.') | (r, c) <- accessible ]
                    in go grid' removed'

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

main :: IO ()
main = do
    args <- getArgs
    progname <- getProgName
    case args of
        [filename] -> do
            content <- readFile filename
            let result = process content
            putStrLn $ "result = " ++ show result
        _ -> usage progname
