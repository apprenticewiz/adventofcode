module Main ( main ) where

import Data.Array
import System.Environment
import System.Exit
import System.IO
import Control.DeepSeq
import System.Clock

parse :: String -> (Int, (Int, Int))
parse content =
    case lines content of
        [depthLine, targetLine] ->
            let depthStr = last $ words depthLine
                targetCoordsStr = last $ words targetLine
                (t1Str, rest) = span (/= ',') targetCoordsStr
                t2Str = case rest of
                    (_:s) -> s
                    [] -> error "malformed target coordinates"
            in (read depthStr, (read t1Str, read t2Str))
        _ -> error "malformed input"

process :: String -> Int
process content =
    let (depth, target@(targetX, targetY)) = parse content
        erosionLevels = makeErosionLevels depth target
        riskLevel = sum [ (erosionLevels ! (x, y)) `mod` 3 | x <- [0..targetX], y <- [0..targetY] ]
    in riskLevel

makeErosionLevels :: Int -> (Int, Int) -> Array (Int, Int) Int
makeErosionLevels depth (targetX, targetY) = erosionLevels
  where
    erosionLevels = array ((0, 0), (targetX, targetY)) [((x, y), erosionLevel x y) | x <- [0..targetX], y <- [0..targetY]]
    
    erosionLevel x y = (geoIndex x y + depth) `mod` 20183
    
    geoIndex x y
      | x == 0 && y == 0 = 0
      | x == targetX && y == targetY = 0
      | y == 0 = x * 16807
      | x == 0 = y * 48271
      | otherwise = (erosionLevels ! (x - 1, y)) * (erosionLevels ! (x, y - 1))

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
