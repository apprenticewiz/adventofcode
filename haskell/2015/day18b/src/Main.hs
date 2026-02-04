module Main (main) where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.Int (Int32)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Control.DeepSeq
import System.Clock

type Grid = UArray (Int, Int) Bool

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

parseInput :: String -> Grid
parseInput content =
    let contentLines = lines content
        numRows = length contentLines
        numCols = length (contentLines !! 0)
        mapChar (row, col) = case (contentLines !! (row - 1)) !! (col -1) of
                                 '#' -> True
                                 '.' -> False
                                 _   -> error ("unexpected char in input at (" ++ show row ++ ", " ++ show col ++ ")")
        initGrid = array ((1, 1), (numRows, numCols)) [((i, j), mapChar (i, j)) | i <- [1..numRows], j <- [1..numCols]]
    in initGrid // [((1, 1), True), ((1, numCols), True), ((numRows, 1), True), ((numRows, numCols), True)]

calcLight :: Grid -> (Int, Int) -> Bool
calcLight grid (row, col) =
    let ((rMin, cMin), (rMax, cMax)) = bounds grid
        onCount = length [() | (dr, dc) <- [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)],
                               let r = row + dr,
                               let c = col + dc,
                               r >= rMin, r <= rMax,
                               c >= cMin, c <= cMax,
                               grid ! (r, c)]
        lightOn = grid ! (row, col)
    in if lightOn then onCount >= 2 && onCount <= 3
                  else onCount == 3

step :: Grid -> Grid
step grid = runSTUArray $ do
                let gridBounds = bounds grid
                let ((rMin, cMin), (rMax, cMax)) = gridBounds
                mgrid <- newArray gridBounds False :: ST s (STUArray s (Int, Int) Bool)
                forM_ (indices grid) $ \i -> do
                    let newVal = calcLight grid i
                    writeArray mgrid i newVal
                writeArray mgrid (rMin, cMin) True
                writeArray mgrid (rMin, cMax) True
                writeArray mgrid (rMax, cMin) True
                writeArray mgrid (rMax, cMax) True
                return mgrid

runSteps :: Int -> Grid -> Grid
runSteps n grid = iterate step grid !! n

countLights :: Grid -> Int
countLights grid = length $ filter id (elems grid)

process :: String -> Int32
process content =
    let grid = parseInput content
    in fromIntegral $ countLights $ runSteps 100 grid


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
