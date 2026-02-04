module Main ( main ) where

import Data.List (isInfixOf)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, readFile, stderr)
import Data.Int (Int32)
import Control.DeepSeq
import System.Clock

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

prop1 :: String -> Bool
prop1 []       = False
prop1 [_]      = False
prop1 (x:y:xs) = [x, y] `isInfixOf` xs || prop1 (y:xs)

prop2 :: String -> Bool
prop2 []         = False
prop2 [_]        = False
prop2 [_, _]     = False
prop2 (x:y:z:xs) = x == z || prop2 (y:z:xs)

process :: String -> Int32
process content =
    let contentLines = lines content
    in sum $ map (\x -> if prop1 x && prop2 x then 1 else 0) contentLines


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
