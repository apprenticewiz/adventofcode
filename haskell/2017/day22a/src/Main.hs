module Main ( main ) where

import Data.Bifunctor
import Data.Set ( Set )
import qualified Data.Set as Set
import System.Environment
import System.Exit
import System.IO

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

process :: String -> Int
process content =
    let grid = lines content
        (rows, cols) = (length grid, length (grid !! 0))
        (cr, cc) = (rows `div` 2, cols `div` 2)
        infected = Set.fromList [ (r - cr, c - cc) | r <- [0..rows - 1]
                                                   , c <- [0..cols - 1]
                                                   , grid !! r !! c == '#' ]
        direction = (-1, 0)
    in go 0 0 (0, 0) direction infected
  where
    go :: Int -> Int -> (Int, Int) -> (Int, Int) -> Set (Int, Int) -> Int
    go 10000 infectionBursts _ _ _ = infectionBursts
    go stepsDone infectionBursts pos dir infected =
        let (dir', infected', infectionBursts') =
                if pos `Set.member` infected
                    then (turnRight dir, Set.delete pos infected, infectionBursts)
                    else (turnLeft dir, Set.insert pos infected, infectionBursts + 1)
            pos' = bimap (fst pos +) (snd pos +) dir'
        in go (stepsDone + 1) infectionBursts' pos' dir' infected'

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
