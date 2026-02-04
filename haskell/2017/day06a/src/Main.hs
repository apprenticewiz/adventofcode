module Main ( main ) where

import Control.Monad.State
import Data.List
import Data.Set ( Set )
import qualified Data.Set as Set
import System.Environment
import System.Exit
import System.IO
import Control.DeepSeq
import System.Clock

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

process :: String -> Int
process content =
    let ns = map read $ words $ head (lines content) :: [Int]
    in evalState findFirstRepeat (Set.empty, 1, ns)
    where
        findFirstRepeat :: State (Set [Int], Int, [Int]) Int
        findFirstRepeat = do
            (seen, current, ns) <- get
            let maxVal = maximum ns
                maxIndex = head $ elemIndices maxVal ns
                newNs = redistribute ns maxIndex
            if Set.member newNs seen
                then return current
                else do
                    put (Set.insert newNs seen, current + 1, newNs)
                    findFirstRepeat
            where
                redistribute :: [Int] -> Int -> [Int]
                redistribute banks idx =
                    let len = length banks
                        blocks = banks !! idx
                        base = take idx banks ++ [0] ++ drop (idx + 1) banks
                        indices = take blocks $ tail $ iterate (\i -> (i + 1) `mod` len) idx
                    in foldl' (\bs i -> adjustAt i (+1) bs) base indices

                adjustAt :: Int -> (a -> a) -> [a] -> [a]
                adjustAt i f xs =
                    let (before, x:after) = splitAt i xs
                    in before ++ (f x : after)


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
