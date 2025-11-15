module Main ( main ) where

import Control.Monad.ST
import Data.Char (digitToInt)
import Data.STRef
import qualified Data.Vector.Unboxed as Vector
import qualified Data.Vector.Unboxed.Mutable as MutableVector
import System.Environment
import System.Exit
import System.IO

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input>"
    exitFailure

matchesAt :: MutableVector.MVector s Int -> Int -> Vector.Vector Int -> ST s Bool
matchesAt recipes pos pattern = do
    let patLen = Vector.length pattern
    let go i
            | i >= patLen = return True
            | otherwise = do
                val <- MutableVector.read recipes (pos + i)
                if val == pattern Vector.! i
                    then go (i + 1)
                    else return False
    go 0

findPattern :: Vector.Vector Int -> ST s Int
findPattern pattern = do
    let patLen = Vector.length pattern
    recipes <- MutableVector.new 30000000
    MutableVector.write recipes 0 3
    MutableVector.write recipes 1 7
    pos1Ref <- newSTRef 0
    pos2Ref <- newSTRef 1
    lenRef <- newSTRef 2
    let loop = do
            pos1 <- readSTRef pos1Ref
            pos2 <- readSTRef pos2Ref
            len <- readSTRef lenRef
            val1 <- MutableVector.read recipes pos1
            val2 <- MutableVector.read recipes pos2
            let !sum' = val1 + val2
            !len' <- if sum' >= 10
                then do
                    MutableVector.write recipes len 1
                    MutableVector.write recipes (len + 1) (sum' `mod` 10)
                    return (len + 2)
                else do
                    MutableVector.write recipes len sum'
                    return (len + 1)
            writeSTRef lenRef len'
            let !pos1' = (pos1 + val1 + 1) `mod` len'
            let !pos2' = (pos2 + val2 + 1) `mod` len'
            writeSTRef pos1Ref pos1'
            writeSTRef pos2Ref pos2'
            if len' >= patLen then do
                match0 <- if len' >= patLen 
                    then matchesAt recipes (len' - patLen) pattern
                    else return False
                match1 <- if len' > patLen && len /= len' && len' - len == 2
                    then matchesAt recipes (len' - patLen - 1) pattern
                    else return False
                if match0 then
                    return (len' - patLen)
                else if match1 then
                    return (len' - patLen - 1)
                else
                    loop
            else
                loop
    loop

process :: String -> Int
process input =
    let pattern = Vector.fromList $ map digitToInt input
    in runST $ findPattern pattern

main :: IO ()
main = do
    args <- getArgs
    progname <- getProgName
    case args of
        [input] -> do
            let result = process input
            putStrLn $ "result = " ++ show result
        _ -> usage progname
