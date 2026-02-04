module Main ( main ) where

import Control.DeepSeq
import qualified Data.Map.Strict as Map
import System.Clock
import System.Environment
import System.Exit
import System.IO

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <number>"
    exitFailure

process :: Int -> Int
process target = go (0, 0) (Map.singleton (0, 0) 1) directions 1
  where
    dirs    = cycle [(1,0), (0,1), (-1,0), (0,-1)]
    lengths = concat [[n,n] | n <- [1..]]
    directions = concat $ zipWith replicate lengths dirs

    go :: (Int,Int) -> Map.Map (Int,Int) Int -> [(Int,Int)] -> Int -> Int
    go _ grid _ val | val > target = val
    go (x,y) grid (dir:rest) _ =
        let (dx, dy) = dir
            pos@(x', y') = (x + dx, y + dy)
            val' = sum [ Map.findWithDefault 0 (x' + i, y' + j) grid
                       | i <- [-1..1], j <- [-1..1], (i,j) /= (0,0) ]
        in go pos (Map.insert pos val' grid) rest val'
    go _ _ [] _ = error "Ran out of directions, which should be impossible."



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
        [n] -> do
            start <- getTime Monotonic
            let result = process (read n)
            result `deepseq` return ()
            end <- getTime Monotonic
            let elapsed = diffTimeSpec start end
            putStrLn $ "result = " ++ show result
            putStrLn $ "elapsed time: " ++ showTime elapsed
        _ -> usage progname
