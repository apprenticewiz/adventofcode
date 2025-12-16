module Main ( main ) where

import Control.DeepSeq
import System.Clock
import System.Environment
import System.Exit
import System.IO

process :: String -> Int
process content =
    let banks = lines content
    in sum $ map findMaxJolts banks

  where
    findMaxJolts :: String -> Int
    findMaxJolts s =
        let first = maximum $ take (length s - 1) s
            s' = drop 1 $ dropWhile (/= first) s
            second = maximum s'
        in read [first, second]

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
