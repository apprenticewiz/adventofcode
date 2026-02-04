module Main ( main ) where

import Control.Monad
import Control.Monad.State
import Data.Array.Unboxed
import System.Environment
import System.Exit
import System.IO
import Control.DeepSeq
import System.Clock

type Grid = UArray (Int, Int) Char

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

simulate :: State Grid ()
simulate = replicateM_ 10 step
  where
    step = do
        grid <- get
        let nextGrid = listArray (bounds grid)
                [ ch' | (x, y) <- indices grid
                      , let ch' = case grid ! (x, y) of
                                '.' -> if countNeighbors grid (x, y) '|' >= 3 then '|' else '.'
                                '|' -> if countNeighbors grid (x, y) '#' >= 3 then '#' else '|'
                                '#' -> if countNeighbors grid (x, y) '#' >= 1 && countNeighbors grid (x, y) '|' >= 1 then '#' else '.'
                                _ -> error "unexpected character in grid"
                ]
        put nextGrid

countNeighbors :: Grid -> (Int, Int) -> Char -> Int
countNeighbors grid (x, y) ch = length [ () | (dx, dy) <- [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)],
                                         let (x', y') = (x + dx, y + dy),
                                         inRange (bounds grid) (x', y'),
                                         grid ! (x', y') == ch
                                       ]

countTrees :: Grid -> Int
countTrees grid = length [ () | (_, ch) <- assocs grid, ch == '|' ]

countLumberyards :: Grid -> Int
countLumberyards grid = length [ () | (_, ch) <- assocs grid, ch == '#' ]

process :: String -> Int
process content =
    let ls = lines content
        maxX = length (head ls) - 1
        maxY = length ls - 1
        grid0 = listArray ((0, 0), (maxX, maxY)) (concat ls)
        finalGrid = execState simulate grid0
        trees = countTrees finalGrid
        lumberyards = countLumberyards finalGrid
    in trees * lumberyards


showTime :: TimeSpec -> String
showTime elapsed =
    let ns = fromIntegral (toNanoSecs elapsed) :: Double
    in if ns < 1000
       then show ns ++ " ns"
       else if ns < 1000000
       then show (ns / 1000.0) ++ " μs"
       else if ns < 1000000000
            then show (ns / 1000000.0) ++ " ms"
            else show (ns / 1000000000.0) ++ " s"
main :: IO ()
main = do
    args <- getArgs
    progname <- getProgName
    case args of
        [filename] -> do
            start <- getTime Monotonic
            content <- readFile filename
            let result = process content
            result `deepseq` return ()
            end <- getTime Monotonic
            let elapsed = diffTimeSpec start end
            putStrLn $ "result = " ++ show result
            putStrLn $ "elapsed time: " ++ showTime elapsed
        _ -> usage progname
