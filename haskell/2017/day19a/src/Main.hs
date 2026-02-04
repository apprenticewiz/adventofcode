module Main ( main ) where

import Data.Array.Unboxed
import System.Environment
import System.Exit
import System.IO
import Control.DeepSeq
import System.Clock

type Position = (Int, Int)

type Direction = (Int, Int)

type Grid = UArray Position Char

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

process :: String -> String
process content =
    let ls = lines content
        height = length ls
        width = length (ls !! 0)
        grid = listArray ((0, 0), (height - 1, width - 1))
                        [ (ls !! r) !! c | r <- [0 .. height - 1], c <- [0 .. width - 1] ]
    in walk grid
  where
    walk :: Grid -> String
    walk grid =
        let (_, (_, maxCol)) = bounds grid
            start = [ (0, j) | j <- [0..maxCol], grid ! (0, j) == '|' ] !! 0
        in go start (1, 0) [] grid

    go :: Position -> Direction -> String -> Grid -> String
    go pos@(r, c) dir@(dr, dc) letters grid
        | not (inRange (bounds grid) pos) = letters
        | grid ! pos == ' '               = letters
        | otherwise =
            case grid ! pos of
                x | x `elem` ['A'..'Z']
                    -> go (r + dr, c + dc) dir (letters ++ [x]) grid
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
                    in go (r + fst validDir, c + snd validDir) validDir letters grid
                _ -> go (r + dr, c + dc) dir letters grid


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
