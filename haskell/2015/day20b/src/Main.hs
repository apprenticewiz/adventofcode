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

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <n>"
    exitFailure

solve :: Int -> Int
solve n = runST $ do
    let limit = n `div` 10
    arr <- newArray (1, limit) 0 :: ST s (STUArray s Int Int)
    forM_ [1..limit] $ \elf -> do
        let gift = elf * 11
            maxHouse = min limit (elf * 50)
        forM_ [elf, 2*elf .. maxHouse] $ \house -> do
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

main :: IO ()
main = do
    args <- getArgs
    progname <- getProgName
    case args of
        [n] -> do
            let result = process (read n)
            putStrLn $ "result = " ++ show result
        _ -> usage progname
