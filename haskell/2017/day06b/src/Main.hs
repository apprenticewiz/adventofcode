module Main ( main ) where

import Control.Monad.State
import Data.List
import Data.Maybe
import System.Environment
import System.Exit
import System.IO

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

process :: String -> Int
process content =
    let ns = map read $ words $ head (lines content) :: [Int]
    in evalState findFirstRepeat ([ns], ns)
    where
        findFirstRepeat :: State ([[Int]], [Int]) Int
        findFirstRepeat = do
            (seen, ns) <- get
            let maxVal = maximum ns
                maxIndex = head $ elemIndices maxVal ns
                newNs = redistribute ns maxIndex
            case elemIndex newNs seen of
                Just firstIndex -> return (length seen - firstIndex)
                Nothing -> do
                    put (seen ++ [newNs], newNs)
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
