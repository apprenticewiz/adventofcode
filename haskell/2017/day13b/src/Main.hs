module Main ( main ) where

import Data.List.Split
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
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
