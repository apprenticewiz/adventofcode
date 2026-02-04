module Main ( main ) where

import Control.DeepSeq
import Data.Array.Unboxed ( UArray )
import qualified Data.Array.Unboxed as Array
import qualified Data.Sequence as Seq
import System.Clock
import System.Environment
import System.Exit
import System.IO

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

process :: String -> Int
process content =
    let ws = words content
        numPlayers = read (ws !! 0) :: Int
        lastMarble = read (ws !! 6) :: Int
        circle = Seq.singleton 0
        scores = Array.array (0, numPlayers - 1) [(i, 0) | i <- [0..numPlayers - 1]] :: UArray Int Int
    in simulate 0 0 numPlayers 1 (lastMarble * 100) circle scores
  where
    simulate currentPos currentPlayer numPlayers currentMarble lastMarble circle scores
        | currentMarble > lastMarble = maximum (Array.elems scores)
        | currentMarble `mod` 23 == 0 =
            let len = Seq.length circle
                ccwPos = (currentPos - 7) `mod` len
                removed = Seq.index circle ccwPos
                newScore = scores Array.! currentPlayer + currentMarble + removed
                scores' = scores Array.// [(currentPlayer, newScore)]
                circle' = Seq.deleteAt ccwPos circle
                newPos = ccwPos `mod` Seq.length circle'
                nextPlayer = (currentPlayer + 1) `mod` numPlayers
            in simulate newPos nextPlayer numPlayers (currentMarble + 1) lastMarble circle' scores'
        | otherwise =
            let len = Seq.length circle
                nextPos = (currentPos + 2) `mod` len
                circle' = Seq.insertAt nextPos currentMarble circle
                nextPlayer = (currentPlayer + 1) `mod` numPlayers
            in simulate nextPos nextPlayer numPlayers (currentMarble + 1) lastMarble circle' scores

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
