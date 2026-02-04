module Main ( main ) where

import Control.DeepSeq
import Control.Monad
import Control.Monad.State
import Data.Array
import Data.Bits
import Data.Char
import Data.List.Split
import Data.Set ( Set )
import Data.Vector ( Vector )
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import System.Clock
import System.Environment
import System.Exit
import System.IO
import Text.Printf

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <key>"
    exitFailure

onesArray :: Array Char [Int]
onesArray = array ('0', 'f') [
    ('0', [0, 0, 0, 0]), ('1', [0, 0, 0, 1]), ('2', [0, 0, 1, 0]), ('3', [0, 0, 1, 1])
  , ('4', [0, 1, 0, 0]), ('5', [0, 1, 0, 1]), ('6', [0, 1, 1, 0]), ('7', [0, 1, 1, 1])
  , ('8', [1, 0, 0, 0]), ('9', [1, 0, 0, 1]), ('a', [1, 0, 1, 0]), ('b', [1, 0, 1, 1])
  , ('c', [1, 1, 0, 0]), ('d', [1, 1, 0, 1]), ('e', [1, 1, 1, 0]), ('f', [1, 1, 1, 1])
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

knot :: String -> Int -> String
knot k n =
    let s = k ++ "-" ++ show n
        lengths = map ord s ++ [17, 31, 73, 47, 23]
        (sparse, _, _) = execState (scramble lengths) (Vector.fromList [0..255], 0, 0)
        chunks = chunksOf 16 (Vector.toList sparse)
        dense = map (foldr1 xor) chunks
    in concatMap (printf "%02x") dense

findRegions :: Array (Int, Int) Int -> [Set (Int, Int)]
findRegions grid = go Set.empty [] where
    allUsed = Set.fromList [ (x,y) | ((x,y),v) <- assocs grid, v == 1 ]

    go visited regions
        | Set.null (allUsed Set.\\ visited) = regions
        | otherwise =
            let start = Set.findMin (allUsed Set.\\ visited)
                region = bfs (Seq.singleton start) Set.empty visited
            in go (visited `Set.union` region) (region : regions)

    bfs Seq.Empty region _ = region
    bfs (q Seq.:|> pos) region visited
        | pos `Set.member` region = bfs q region visited
        | otherwise =
            let (x,y) = pos
                nbrs = [ (nx,ny)
                        | (dx , dy) <- [(-1, 0), (1, 0), (0, -1), (0, 1)]
                        , let (nx,ny) = (x + dx, y + dy)
                        , inRange (bounds grid) (nx, ny)
                        , grid ! (nx,ny) == 1
                        , not ((nx,ny) `Set.member` region)
                        ]
            in bfs (q Seq.>< Seq.fromList nbrs) (Set.insert pos region) visited           

process :: String -> Int
process key =
    let hashes = map (knot key) [0..127]
        bits = concatMap (concatMap (onesArray !)) hashes
        grid = listArray ((0,0),(127,127)) bits
        regions = findRegions grid
    in length regions

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
