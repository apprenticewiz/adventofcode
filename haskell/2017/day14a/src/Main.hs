module Main ( main ) where

import Control.Monad
import Control.Monad.State
import Data.Array
import Data.Bits
import Data.Char
import Data.List.Split
import Data.Vector ( Vector )
import qualified Data.Vector as Vector
import System.Environment
import System.Exit
import System.IO
import Text.Printf
import Control.DeepSeq
import System.Clock

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <key>"
    exitFailure

onesArray :: Array Char Int
onesArray = array ('0', 'f') [
    ('0', 0), ('1', 1), ('2', 1), ('3', 2), ('4', 1), ('5', 2), ('6', 2), ('7', 3)
  , ('8', 1), ('9', 2), ('a', 2), ('b', 3), ('c', 2), ('d', 3), ('e', 3), ('f', 4)
  ]

scramble :: [Int] -> State (Vector Int, Int, Int) ()
scramble lens = do
    replicateM_ 64 $ forM_ lens $ \len -> modify (`step` len)
  where
    step :: (Vector Int, Int, Int) -> Int -> (Vector Int, Int, Int)
    step (ns, pos, skip) len =
        let n = Vector.length ns
            idxs = Vector.fromList $ map (`mod` n) [pos .. pos + len - 1]
            vals = Vector.map (ns Vector.!) idxs
            revs = Vector.reverse vals
            ns' = foldl (\acc (i, v) -> Vector.take i acc Vector.++ Vector.singleton v Vector.++ Vector.drop (i + 1) acc) ns (Vector.zip idxs revs)
            pos' = (pos + len + skip) `mod` n
        in (ns', pos', skip + 1)

process :: String -> Int
process key =
    let hashes = map (knot key) [0..127]
    in sum $ map countOnes hashes
    where
        knot :: String -> Int -> String
        knot k n =
            let s = k ++ "-" ++ show n
                lengths = map ord s ++ [17, 31, 73, 47, 23]
                (sparse, _, _) = execState (scramble lengths) (Vector.fromList [0..255], 0, 0)
                chunks = chunksOf 16 (Vector.toList sparse)
                dense = map (foldr1 xor) chunks
            in concatMap (printf "%02x") dense

        countOnes :: String -> Int
        countOnes s = sum $ map (onesArray !) s


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
        [key] -> do
            start <- getTime Monotonic
            let result = process key
            result `deepseq` return ()
            end <- getTime Monotonic
            let elapsed = diffTimeSpec start end
            putStrLn $ "result = " ++ show result
            putStrLn $ "elapsed time: " ++ showTime elapsed
        _ -> usage progname
