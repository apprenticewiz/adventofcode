module Main ( main ) where

import Control.DeepSeq
import Data.Ix
import Data.IntSet ( IntSet )
import qualified Data.IntSet as IntSet
import System.Clock
import System.Environment
import System.Exit
import System.IO


decode :: Int -> (Int, Int)
decode n = (n `div` 1000, n `mod` 1000)

encode :: (Int, Int) -> Int
encode (r, c) = r * 1000 + c

process :: String -> Int
process content =
    case lines content of
        [] -> error "empty input"
        ls@(row0:_) ->
            let rows = length ls
                cols = length row0
                bounds = ((0, 0), (rows - 1, cols - 1))
                rolls = IntSet.fromList [ encode (r, c) | (r, line) <- zip [0..] ls
                                                        , (c, ch)  <- zip [0..] line
                                                        , ch == '@'
                                        ]
            in prune rolls bounds
  where
    prune :: IntSet -> ((Int, Int), (Int, Int)) -> Int
    prune rolls bounds =
        let neighbors (r, c) = [ (nr, nc) | dr <- [-1, 0, 1] 
                                         , dc <- [-1, 0, 1]
                                         , (dr, dc) /= (0, 0)
                                         , let nr = r + dr
                                         , let nc = c + dc
                                         , inRange bounds (nr, nc)
                                         , encode (nr, nc) `IntSet.member` rolls
                              ]
            removable = IntSet.filter (\pt -> length (neighbors (decode pt)) < 4) rolls
        in if IntSet.null removable
           then IntSet.size rolls
           else prune (rolls `IntSet.difference` removable) bounds

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

showTime :: TimeSpec -> String
showTime elapsed =
    let ns = fromIntegral (toNanoSecs elapsed) :: Double
    in if ns < 1000
       then show ns ++ " ns"
       else if ns < 1000000
       then show (ns / 1000.0) ++ " μs"
       else if ns < 10000000000
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
