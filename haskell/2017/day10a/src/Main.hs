module Main ( main ) where

import Control.DeepSeq
import Control.Monad
import Control.Monad.State
import Data.List.Split
import System.Clock
import System.Environment
import System.Exit
import System.IO

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

scramble :: [Int] -> State ([Int], Int, Int) ()
scramble lens = do
    forM_ lens $ \len -> modify (\s -> step s len)
    where
        step :: ([Int], Int, Int) -> Int -> ([Int], Int, Int)
        step (ns, pos, skip) len =
            let n = length ns
                idxs = map (`mod` n) [pos .. pos + len - 1]
                vals = map (ns !!) idxs
                revs = reverse vals
                ns' = foldl (\acc (i, v) -> take i acc ++ [v] ++ drop (i + 1) acc) ns (zip idxs revs)
                pos' = (pos + len + skip) `mod` n
            in (ns', pos', skip + 1)

process :: String -> Int
process content =
    case lines content of
        [line] ->
            let input = map read (splitOn "," line)
                (final, _, _) = execState (scramble input) ([0..255], 0, 0)
            in product $ take 2 $ final
        _ -> error "malformed input - expected single line of data"

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
