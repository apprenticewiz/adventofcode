module Main ( main ) where

import Data.Array.Unboxed ( UArray )
import qualified Data.Array.Unboxed as Array
import System.Environment
import System.Exit
import System.IO
import Control.DeepSeq
import System.Clock

gridDimensions :: (Int, Int)
gridDimensions = (300, 300)

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <serial>"
    exitFailure

process :: Int -> (Int, Int)
process serial =
    let (maxX, maxY) = gridDimensions
        grid = Array.array ((1, 1), gridDimensions) [ ((x, y), n) | x <- [1..maxX], y <- [1..maxY], let n = calcPower (x, y) ]
    in findBestPower grid minBound (0, 0) [ (x, y) | x <- [1..maxX - 2], y <- [1..maxY - 2] ]
  where
    calcPower :: (Int, Int) -> Int
    calcPower (x, y) =
        let rackId = x + 10
            power = ((rackId * y) + serial) * rackId
            hundreds = (power `div` 100) `mod` 10
        in hundreds - 5

    findBestPower :: UArray (Int, Int) Int -> Int -> (Int, Int) -> [(Int, Int)] -> (Int, Int)
    findBestPower grid bestPower bestPos coords
      | null coords = bestPos
      | otherwise =
            let (currPos:rest) = coords
                (x, y) = currPos
                squarePower = sum [ grid Array.! (i, j) | i <- [x..x+2], j <- [y..y+2] ]
            in if squarePower > bestPower
                then findBestPower grid squarePower currPos rest
                else findBestPower grid bestPower bestPos rest


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
        [input] -> do
            let serial = read input
            start <- getTime Monotonic
                result = process serial
            result `deepseq` return ()
            end <- getTime Monotonic
            let elapsed = diffTimeSpec start end
            putStrLn $ "result = " ++ show result
            putStrLn $ "elapsed time: " ++ showTime elapsed
        _ -> usage progname
