module Main ( main ) where

import Control.DeepSeq
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import System.Clock
import System.Environment
import System.Exit
import System.IO

process :: String -> Int
process content =
    let parentMap = foldr insertParent Map.empty (lines content)
    in sum $ map (countOrbits parentMap) (Map.keys parentMap)
  where
    insertParent :: String -> Map String String -> Map String String
    insertParent line orbits =
        case span (/= ')') line of
            (parent, _:child) -> Map.insert child parent orbits
            _ -> error ("malformed input line: " ++ line)

    countOrbits :: Map String String -> String -> Int
    countOrbits parents node =
        case Map.lookup node parents of
            Just parent -> 1 + countOrbits parents parent
            Nothing -> 0

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
