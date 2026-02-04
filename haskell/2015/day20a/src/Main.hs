{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Int (Int32)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Control.DeepSeq
import System.Clock

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <n>"
    exitFailure

solve :: Int -> Int
solve n = runST $ do
    let limit = n `div` 10
    arr <- newArray (1, limit) 0 :: ST s (STUArray s Int Int)
    forM_ [1..limit] $ \elf -> do
        let gift = elf * 10
        forM_ [elf, 2*elf .. limit] $ \house -> do
            cur <- readArray arr house
            writeArray arr house (cur + gift)
    let go !i
          | i > limit = error "unreachable"
          | otherwise = do
              p <- readArray arr i
              if p >= n then return i else go (i + 1)
    go 1

process :: Int -> Int32
process = fromIntegral . solve


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
        [n] -> do
            start <- getTime Monotonic
            let result = process (read n)
            result `deepseq` return ()
            end <- getTime Monotonic
            let elapsed = diffTimeSpec start end
            putStrLn $ "result = " ++ show result
            putStrLn $ "elapsed time: " ++ showTime elapsed
        _ -> usage progname
