module Main ( main ) where

import Control.DeepSeq
import Data.List
import Data.Set ( Set )
import qualified Data.Set as Set
import System.Clock
import System.Environment
import System.Exit
import System.IO

type Position = (Int, Int, Int)

process :: String -> Int
process content =
    let points = map parsePoint (lines content)
        circuits = [ Set.singleton p | p <- points ]
        distances = [ ((p1, p2), d) | p1 <- points, p2 <- points, p1 < p2, let d = euclidian p1 p2 ]
        closest = map fst $ sortOn snd distances
        connectIterations = scanl' connect circuits closest
        [c1, c2] = head $ filter (\l -> length l == 2) connectIterations
        (p1, p2) = head [ (a, b) | (a, b) <- closest,
                                   (a `Set.member` c1 && b `Set.member` c2) ||
                                   (b `Set.member` c1 && a `Set.member` c2) ]
        (x1, _, _) = p1
        (x2, _, _) = p2
    in x1 * x2

  where
    parsePoint :: String -> Position
    parsePoint line =
        let (xStr, _:line') = span (/= ',') line
            (yStr, _:zStr) = span (/= ',') line'
        in (read xStr, read yStr, read zStr)

    euclidian :: Position -> Position -> Double
    euclidian (x1, y1, z1) (x2, y2, z2) =
        let x1' = fromIntegral x1 :: Double
            y1' = fromIntegral y1 :: Double
            z1' = fromIntegral z1 :: Double
            x2' = fromIntegral x2 :: Double
            y2' = fromIntegral y2 :: Double
            z2' = fromIntegral z2 :: Double
            xx = (x1' - x2') * (x1' - x2')
            yy = (y1' - y2') * (y1' - y2')
            zz = (z1' - z2') * (z1' - z2')
        in sqrt (xx + yy + zz)

    connect :: [Set Position] -> (Position, Position) -> [Set Position]
    connect circuits (p1, p2) =
        let c1 = head [ c | c <- circuits, p1 `Set.member` c ]
            c2 = head [ c | c <- circuits, p2 `Set.member` c ]
            circuits' = circuits \\ [c1, c2]
            joined = Set.union c1 c2
        in circuits' ++ [joined]

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
