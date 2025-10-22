module Main ( main ) where

import Data.Bits
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import qualified Data.Set as Set
import Data.Set (Set)
import System.Environment
import System.Exit
import System.IO

type Coordinate = (Int, Int)

data Cell = Wall | Open
            deriving (Eq, Show)

startPos :: Coordinate
startPos = (1, 1)

numSteps :: Int
numSteps = 50

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
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
    bfs Seq.Empty visited = Set.size visited
    bfs queue visited     =
        let ((current, steps) Seq.:< rest) = Seq.viewl queue
        in if steps == numSteps
           then bfs rest visited
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

main :: IO ()
main = do
    args <- getArgs
    progname <- getProgName
    case args of
        [n] -> do
            let result = process (read n :: Int)
            putStrLn $ "result = " ++ show result
        _ -> usage progname
