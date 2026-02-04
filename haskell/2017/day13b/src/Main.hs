module Main ( main ) where

import Control.DeepSeq
import Data.List.Split
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import System.Clock
import System.Environment
import System.Exit
import System.IO

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

findDelay :: Map Int Int -> Int
findDelay firewall =
    let nextCatch = Map.fromList [ (d, p) | (d, r) <- Map.toList firewall
                                          , let p = (r - 1) * 2
                                 ]
        go delay =
            let catches = Map.mapWithKey (\depth period -> (depth + delay) `mod` period == 0) nextCatch
            in if not (or catches)
                then delay
                else go (delay + 1)
    in go 0

process :: String -> Int
process content =
    let firewall = Map.fromList $ foldr (\line acc ->
                        let parts = splitOn ": " line
                            [x, y] = map read parts
                        in ((x, y):acc)
                    ) [] (lines content)
    in findDelay firewall

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
