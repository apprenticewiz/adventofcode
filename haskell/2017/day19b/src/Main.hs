module Main ( main ) where

import Data.Array.Unboxed
import System.Environment
import System.Exit
import System.IO

type Position = (Int, Int)

type Direction = (Int, Int)

type Grid = UArray Position Char

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

process :: String -> Int
process content =
    let ls = lines content
        height = length ls
        width = length (ls !! 0)
        grid = listArray ((0, 0), (height - 1, width - 1))
                        [ (ls !! r) !! c | r <- [0 .. height - 1], c <- [0 .. width - 1] ]
    in walk grid
  where
    walk :: Grid -> Int
    walk grid =
        let (_, (_, maxCol)) = bounds grid
            start = [ (0, j) | j <- [0..maxCol], grid ! (0, j) == '|' ] !! 0
        in go start (1, 0) 0 grid

    go :: Position -> Direction -> Int -> Grid -> Int
    go pos@(r, c) dir@(dr, dc) steps grid
        | not (inRange (bounds grid) pos) = steps
        | grid ! pos == ' '               = steps
        | otherwise =
            case grid ! pos of
                '+' ->
                    let nextDirs = case dir of
                            (1, 0)  -> [(0, -1), (0, 1)]
                            (-1, 0) -> [(0, -1), (0, 1)]
                            (0, 1)  -> [(-1, 0), (1, 0)]
                            (0, -1) -> [(-1, 0), (1, 0)]
                            _ -> error "invalid dir"
                        validDir = [ d | d@(dr', dc') <- nextDirs
                                       , let p' = (r + dr', c + dc')
                                       , inRange (bounds grid) p'
                                       , grid ! p' /= ' ' ] !! 0
                    in go (r + fst validDir, c + snd validDir) validDir (steps + 1) grid
                _ -> go (r + dr, c + dc) dir (steps + 1) grid

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
