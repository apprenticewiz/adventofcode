module Main ( main ) where

import Control.DeepSeq
import Data.List
import Data.Maybe
import System.Clock
import System.Environment
import System.Exit
import System.IO

process :: String -> Integer
process content =
    let banks = lines content
    in sum $ map findMaxJolts banks

  where
    findMaxJolts :: String -> Integer
    findMaxJolts = go 12 []

      where
        go :: Int -> String -> String -> Integer
        go numDigits acc s
          | null s || numDigits == 0 = read $ reverse acc
          | otherwise =
            let next = maximum $ take (length s - (numDigits - 1)) s
                nextPos = fromJust $ next `elemIndex` s
            in go (numDigits - 1) (next : acc) (drop (nextPos + 1) s)

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
