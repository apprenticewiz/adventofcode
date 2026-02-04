module Main ( main ) where

import Control.DeepSeq
import Data.List.Split
import qualified Data.Map.Strict as Map
import System.Clock
import System.Environment
import System.Exit
import System.IO

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

process :: String -> Int
process content =
    let firewall = Map.fromList $ foldr (\line acc ->
                        let parts = splitOn ": " line
                            [x, y] = map read parts
                        in ((x, y):acc)
                    ) [] (lines content)
    in foldr (\depth acc ->
            let range = firewall Map.! depth
                cycleTime = (range - 1) * 2
            in if depth `mod` cycleTime == 0
                then acc + depth * range
                else acc
        ) 0 (Map.keys firewall)

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
