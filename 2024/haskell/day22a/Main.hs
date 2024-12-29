module Main ( main ) where

import Data.Bits ( xor )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure )

usage :: IO ()
usage = do
    progname <- getProgName
    putStrLn $ "usage: " ++ progname ++ " <file>"
    exitFailure

mix :: Int -> Int -> Int
mix x y = x `xor` y

prune :: Int -> Int
prune x = x `mod` 16777216

step :: Int -> Int
step = step3 . step2 . step1
    where
        step1 x = prune $ mix x (x * 64)
        step2 x = prune $ mix x (x `div` 32)
        step3 x = prune $ mix x (x * 2048)

process :: String -> Int
process contents =
    let numbers = map read (lines contents) :: [Int]
    in sum $ map (\x -> iterate step x !! 2000) numbers

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            contents <- readFile filename
            let result = process contents
            putStrLn $ "result = " ++ show result
        _ -> usage
