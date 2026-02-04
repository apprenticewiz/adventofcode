module Main ( main ) where

import Control.DeepSeq
import Data.Bits
import Data.Sequence (Seq)
import Data.Set (Set)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import System.Clock
import System.Environment
import System.Exit
import System.IO

type Coordinate = (Int, Int)

data Cell = Wall | Open
            deriving (Eq, Show)

startPos :: Coordinate
startPos = (1, 1)

endPos :: Coordinate
endPos = (31, 39)

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <n>"
    exitFailure

coordToCell :: Int -> Coordinate -> Cell
coordToCell n (x, y) =
    let v = x*x + 3*x + 2*x*y + y + y*y + n
        bits = popCount v
    in if odd bits then Wall else Open

process :: Int -> Int  
process n = bfs (Seq.singleton (startPos, 0)) (Set.singleton startPos)
  where
    bfs :: Seq (Coordinate, Int) -> Set Coordinate -> Int
    bfs Seq.Empty _   = error "No path found"
    bfs queue visited =
        let ((current, steps) Seq.:< rest) = Seq.viewl queue
        in if current == endPos
           then steps
           else
               let neighbors = filter isValidNeighbor (getNeighbors current)
                   newNeighbors = filter (`Set.notMember` visited) neighbors
                   newVisited = foldr Set.insert visited newNeighbors
                   newQueue = rest Seq.>< Seq.fromList [(coord, steps + 1) | coord <- newNeighbors]
               in bfs newQueue newVisited

    isValidNeighbor :: Coordinate -> Bool
    isValidNeighbor coord@(x, y) = x >= 0 && y >= 0 && coordToCell n coord == Open

    getNeighbors :: Coordinate -> [Coordinate]
    getNeighbors (x, y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

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
            let result = process (read n :: Int)
            result `deepseq` return ()
            end <- getTime Monotonic
            let elapsed = diffTimeSpec start end
            putStrLn $ "result = " ++ show result
            putStrLn $ "elapsed time: " ++ showTime elapsed
        _ -> usage progname
