module Main ( main ) where

import Data.List.Split
import qualified Data.Map.Strict as Map
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
