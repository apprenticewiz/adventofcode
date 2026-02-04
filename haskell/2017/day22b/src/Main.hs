module Main ( main ) where

import Data.Bifunctor
import Data.Map.Strict ( Map )
import qualified Data.Map as Map
import System.Environment
import System.Exit
import System.IO
import Control.DeepSeq
import System.Clock

data NodeStatus = Clean | Weakened | Infected | Flagged
                  deriving (Eq, Show)

type StatusMap = Map (Int, Int) NodeStatus

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

turnLeft :: (Int, Int) -> (Int, Int)
turnLeft (-1, 0) = (0, -1)
turnLeft (0, -1) = (1, 0)
turnLeft (1, 0) = (0, 1)
turnLeft (0, 1) = (-1, 0)
turnLeft _ = error "undefined"

turnRight :: (Int, Int) -> (Int, Int)
turnRight (-1, 0) = (0, 1)
turnRight (0, 1) = (1, 0)
turnRight (1, 0) = (0, -1)
turnRight (0, -1) = (-1, 0)
turnRight _ = error "undefined"

turnAround :: (Int, Int) -> (Int, Int)
turnAround (-1, 0) = (1, 0)
turnAround (0, -1) = (0, 1)
turnAround (1, 0) = (-1, 0)
turnAround (0, 1) = (0, -1)
turnAround _ = error "undefined"

process :: String -> Int
process content =
    let grid = lines content
        (rows, cols) = (length grid, length (grid !! 0))
        (cr, cc) = (rows `div` 2, cols `div` 2)
        statusMap = Map.fromList [ ((r - cr, c - cc), status)
                                 | r <- [0..rows - 1]
                                 , c <- [0..cols - 1]
                                 , let status = if grid !! r !! c == '#' then Infected else Clean ]
        direction = (-1, 0)
    in go 0 0 (0, 0) direction statusMap
  where
    go :: Int -> Int -> (Int, Int) -> (Int, Int) -> StatusMap -> Int
    go 10000000 infectionBursts _ _ _ = infectionBursts
    go stepsDone infectionBursts pos dir statusMap =
        let (dir', statusMap', infectionBursts') =
                case Map.findWithDefault Clean pos statusMap of
                    Clean -> (turnLeft dir, Map.insert pos Weakened statusMap, infectionBursts)
                    Weakened -> (dir, Map.insert pos Infected statusMap, infectionBursts + 1)
                    Infected -> (turnRight dir, Map.insert pos Flagged statusMap, infectionBursts)
                    Flagged -> (turnAround dir, Map.insert pos Clean statusMap, infectionBursts)
            pos' = bimap (fst pos +) (snd pos +) dir'
        in go (stepsDone + 1) infectionBursts' pos' dir' statusMap'


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
