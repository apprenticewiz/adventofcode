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

deltaMap :: Map String (Int, Int, Int)
deltaMap = Map.fromList
    [ ("n",  ( 0,  1,  1))
    , ("ne", ( 1,  1,  0))
    , ("se", ( 1,  0, -1))
    , ("s",  ( 0, -1, -1))
    , ("sw", (-1, -1,  0))
    , ("nw", (-1,  0,  1))
    ]

process :: String -> Int
process content =
    case lines content of
        [line] ->
            let dirs = splitOn "," line
                (_, maxDist) = foldl' step ((0, 0, 0), 0) dirs
            in maxDist
            where
                step :: ((Int, Int, Int), Int) -> String -> ((Int, Int, Int), Int)
                step ((p, q, r), maxDist) dir =
                    let (dp, dq, dr) = deltaMap Map.! dir
                        (p', q', r') = (p + dp, q + dq, r + dr)
                        maxDist' = max maxDist ((abs p' + abs q' + abs r') `div` 2)
                    in ((p', q', r'), maxDist')
        _ -> error "malformed input: data expected on a single line"

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
