module Main ( main ) where

import Control.DeepSeq
import System.Clock
import System.Environment
import System.Exit
import System.IO

process :: String -> Int
process range =
    case span (/= '-') range of
        (loStr, '-':hiStr) ->
            let lo = read loStr
                hi = read hiStr
            in length [ n | n <- [lo..hi], isValid n ]
        _ -> error "invalid range"
    where
        isValid :: Int -> Bool
        isValid n = go n 10 False
          where
            go :: Int -> Int -> Bool -> Bool
            go 0 _ hasDouble = hasDouble
            go m prevDigit hasDouble =
                let d = m `mod` 10
                    m' = m `div` 10
                in ((d <= prevDigit) && go m' d (hasDouble || d == prevDigit))

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <range>"
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
        [range] -> do
            start <- getTime Monotonic
            let result = process range
            result `deepseq` return ()
            end <- getTime Monotonic
            let elapsed = diffTimeSpec start end
            putStrLn $ "result = " ++ show result
            putStrLn $ "elapsed time: " ++ showTime elapsed
        _ -> usage progname
