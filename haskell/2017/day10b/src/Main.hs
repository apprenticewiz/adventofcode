module Main ( main ) where

import Control.Monad
import Control.Monad.State
import Data.Bits
import Data.Char
import Data.List.Split
import System.Environment
import System.Exit
import System.IO
import Text.Printf

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

scramble :: [Int] -> State ([Int], Int, Int) ()
scramble lens = do
    replicateM_ 64 $
        forM_ lens $ \len ->
            modify (\s -> step s len)
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

process :: String -> String
process content =
    case lines content of
        [line] ->
            let input = (map ord line) ++ [17, 31, 73, 47, 23]
                (sparse, _, _) = execState (scramble input) ([0..255], 0, 0)
                chunks = chunksOf 16 sparse
                dense = map (foldr1 xor) chunks
            in concatMap (printf "%02x") dense
        _ -> error "malformed input - expected single line of data"

main :: IO ()
main = do
    args <- getArgs
    progname <- getProgName
    case args of
        [filename] -> do
            content <- readFile filename
            let result = process content
            putStrLn $ "result = " ++ show result
        _ -> usage progname
