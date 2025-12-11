module Main ( main ) where

import Control.DeepSeq
import Control.Monad.State.Strict
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import System.Clock
import System.Environment
import System.Exit
import System.IO

type Graph = Map String [String]
type Memo = Map String Integer

parseLine :: String -> (String, [String])
parseLine line =
    let (k, _:rest) = span (/= ':') line
        vs          = words rest
    in (k, vs)

countPaths :: Graph -> String -> State Memo Integer
countPaths graph = go
  where
    go :: String -> State Memo Integer
    go node
        | node == "out" = return 1
        | otherwise = do
            memo <- get
            case Map.lookup node memo of
                Just v  -> return v
                Nothing -> do
                    let neighbors = Map.findWithDefault [] node graph
                    vals <- mapM go neighbors
                    let total = sum vals
                    modify' (Map.insert node total)
                    return total

process :: String -> Integer
process content =
    let graph = Map.fromList (map parseLine $ filter (not . null) $ lines content)
    in evalState (countPaths graph "you") Map.empty

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
