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
type MemoKey = (String, Bool, Bool)
type Memo = Map MemoKey Integer

parseLine :: String -> (String, [String])
parseLine line =
    let (k, _:rest) = span (/= ':') line
        vs          = words rest
    in (k, vs)

countPaths :: Graph -> String -> State Memo Integer
countPaths graph start = go start False False
  where
    go :: String -> Bool -> Bool -> State Memo Integer
    go node hasDac hasFft
        | node == "out" = return (if hasDac && hasFft then 1 else 0)
        | otherwise = do
            let hasDac' = hasDac || node == "dac"
                hasFft' = hasFft || node == "fft"
                key = (node, hasDac', hasFft')

            memo <- get
            case Map.lookup key memo of
                Just v  -> return v
                Nothing -> do
                    let neighbors = Map.findWithDefault [] node graph
                    vals <- mapM (\n -> go n hasDac' hasFft') neighbors
                    let total = sum vals
                    modify' (Map.insert key total)
                    return total

process :: String -> Integer
process content =
    let graph = Map.fromList (map parseLine $ filter (not . null) $ lines content)
    in evalState (countPaths graph "svr") Map.empty

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
