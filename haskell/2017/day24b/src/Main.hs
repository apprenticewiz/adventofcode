module Main ( main ) where

import Control.DeepSeq
import Data.List.Split
import System.Clock
import System.Environment
import System.Exit
import System.IO

type Component = (Int, Int)

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

scanComponents :: String -> [Component]
scanComponents content = map scanComponent (lines content)
  where
    scanComponent line =
        let [a, b] = map read $ splitOn "/" line
        in (a, b)

buildBridges :: [(Int, Int)] -> Int -> [[(Int, Int)]]
buildBridges components port =
    [ comp : rest | comp@(a, b) <- components
                  , port == a || port == b
                  , let nextPort = if port == a then b else a
                  , rest <- buildBridges (remove comp components) nextPort
    ] ++ [[]]
  where
    remove x = filter (/= x)

strength :: [(Int, Int)] -> Int
strength = sum . map (uncurry (+))

process :: String -> Int
process content =
    let components = scanComponents content
    in (snd . maximum) [ (length bridge, strength bridge) | bridge <- buildBridges components 0 ]

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
