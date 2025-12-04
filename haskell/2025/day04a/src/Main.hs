module Main ( main ) where

import Control.DeepSeq
import Data.Ix
import qualified Data.Set as Set
import System.Clock
import System.Environment
import System.Exit
import System.IO

process :: String -> Int
process content =
    let ls = lines content
    in case ls of
        [] -> error "empty input"
        (first:_) ->
            let rows = length ls
                cols = length first
                bounds = ((0, 0), (rows - 1, cols - 1))
                rolls = Set.fromList[ (r, c) | r <- [0 .. rows - 1], c <- [0 .. cols - 1], ls !! r !! c == '@' ]
                neighbors (r, c) =
                    [ neighbor | (dr, dc) <- [ (-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1) ]
                               , let neighbor = (r + dr, c + dc)
                               , bounds `inRange` (r + dr, c + dc)
                               , neighbor `Set.member` rolls
                    ]
                accessible = Set.filter (\roll -> length (neighbors roll) < 4) rolls
            in length accessible

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
       else if ns < 10000000000
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