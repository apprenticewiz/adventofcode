module Main ( main ) where

import Control.Monad.ST
import Data.Array.MArray
import Data.Array.ST
import System.Environment
import System.Exit
import System.IO

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

process :: String -> Int
process content =
    let offsets = (map read $ lines content) :: [Int]
    in runST $ do
        os <- newListArray (0, length offsets - 1) offsets :: ST s (STUArray s Int Int)
        go os 0 0
    where
        go :: STUArray s Int Int -> Int -> Int -> ST s Int
        go os i steps = do
            (lower, upper) <- getBounds os
            if i < lower || i > upper
                then return steps
                else do
                   offset <- readArray os i
                   writeArray os i (offset + 1)
                   go os (i + offset) (steps + 1)

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
