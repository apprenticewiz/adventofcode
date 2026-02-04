module Main ( main ) where

import System.Environment
import System.Exit
import System.IO
import Control.DeepSeq
import System.Clock

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <step> <insertions>"
    exitFailure

process :: Int -> Int -> Int
process step insertions =
    go 0 1 [0]
  where
    go pos n buf
        | n > insertions = 
            let nextPos = (pos + 1) `mod` length buf
            in buf !! nextPos
        | otherwise =
            let pos' = (pos + step) `mod` length buf + 1
                buf' = take pos' buf ++ [n] ++ drop pos' buf
            in go pos' (n + 1) buf'


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
        [step, insertions] -> do
            start <- getTime Monotonic
            let result = process (read step) (read insertions)
            result `deepseq` return ()
            end <- getTime Monotonic
            let elapsed = diffTimeSpec start end
            putStrLn $ "result = " ++ show result
            putStrLn $ "elapsed time: " ++ showTime elapsed
        _ -> usage progname
