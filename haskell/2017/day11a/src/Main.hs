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
                (p, q, r) = foldl' step (0, 0, 0) dirs
            in (abs p + abs q + abs r) `div` 2
            where
                step :: (Int, Int, Int) -> String -> (Int, Int, Int)
                step (p, q, r) dir =
                    let (dp, dq, dr) = deltaMap Map.! dir
                    in (p + dp, q + dq, r + dr)
        _ -> error "malformed input: data expected on a single line"

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
