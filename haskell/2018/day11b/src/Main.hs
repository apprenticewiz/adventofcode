module Main ( main ) where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed ( UArray )
import qualified Data.Array.Unboxed as Array
import Data.List
import Data.Ord
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

buildGrid :: Int -> UArray (Int, Int) Int
buildGrid serial =
    let (maxX, maxY) = gridDimensions
    in Array.array ((1, 1),(maxX, maxY))
         [ ((x, y), power serial x y)
         | x <- [1..maxX], y <- [1..maxY] ]

power :: Int -> Int -> Int -> Int
power serial x y =
    let rack = x + 10
        p    = (rack * y + serial) * rack
    in (p `div` 100 `mod` 10) - 5

buildSAT :: UArray (Int,Int) Int -> UArray (Int,Int) Int
buildSAT grid = runSTUArray $ do
    let ((x1, y1),(x2, y2)) = Array.bounds grid
    sat <- newArray ((x1, y1),(x2, y2)) 0 :: ST s (STUArray s (Int,Int) Int)
    forM_ [x1..x2] $ \x ->
        forM_ [y1..y2] $ \y -> do
            let g = grid Array.! (x, y)
            left  <- if x > x1 then readArray sat (x-1, y) else return 0
            above <- if y > y1 then readArray sat (x, y-1) else return 0
            diag  <- if x > x1 && y > y1 then readArray sat (x-1, y-1) else return 0
            writeArray sat (x, y) (g + left + above - diag)
    return sat

squareSum :: UArray (Int, Int) Int -> Int -> Int -> Int -> Int
squareSum sat x y s =
      sat Array.! (x2, y2)
    - satAt (x1, y2)
    - satAt (x2 ,y1)
    + satAt (x1, y1)
  where
    (x1, y1) = (x-1, y-1)
    (x2, y2) = (x+s-1, y+s-1)
    ((lx, ly), _) = Array.bounds sat

    satAt (i,j)
        | i < lx || j < ly = 0
        | otherwise        = sat Array.! (i, j)

bestForSize :: UArray (Int, Int) Int -> Int -> Int -> Int -> (Int, Int, Int, Int)
bestForSize sat s maxX maxY =
    maximumBy (comparing (\(_ ,_ ,_ , p)->p))
      [ (x,y,s, squareSum sat x y s)
      | x <- [1 .. maxX - s + 1]
      , y <- [1 .. maxY - s + 1]
      ]

process :: Int -> (Int, Int, Int)
process serial =
    let (maxX, maxY) = gridDimensions
        grid = buildGrid serial
        sat  = buildSAT grid
        candidates =
            [ bestForSize sat n maxX maxY
            | n <- [1..maxX] ]
        (x, y, s, _) = maximumBy (comparing (\(_ ,_ ,_ ,p) -> p)) candidates
    in (x, y, s)


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
