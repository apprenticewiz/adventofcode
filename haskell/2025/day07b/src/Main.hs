module Main ( main ) where

import Control.DeepSeq
import Data.Ix
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Data.Set ( Set )
import qualified Data.Set as Set
import System.Clock
import System.Environment
import System.Exit
import System.IO

type Position = (Int, Int)

process :: String -> Int
process content =
    let ls = lines content
        rows = length ls
        cols = length (head ls)
        bounds = ((0, 0), (rows - 1, cols - 1))
        start = head [ (r, c) | r <- [0..rows - 1], c <- [0..cols - 1], ls !! r !! c == 'S' ]
        splitters = Set.fromList [ (r, c) | r <- [0..rows - 1], c <- [0..cols - 1], ls !! r !! c == '^' ]
        (count, _) = simulate bounds splitters Map.empty Set.empty start
    in count
  where
    simulate :: ((Int, Int), (Int, Int)) -> Set Position -> Map Position Int -> Set Position -> Position -> (Int, Map Position Int)
    simulate bounds splitters cache visiting pos
      | not (inRange bounds pos) = (1, cache)
      | Just v <- Map.lookup pos cache = (v, cache)
      | pos `Set.member` visiting = error ("cycle detected in manifold at " ++ show pos ++ " — infinite timelines")
      | otherwise =
          let visiting' = Set.insert pos visiting
              (r, c) = pos
          in if pos `Set.member` splitters
                then let (leftCount, cache')  = simulate bounds splitters cache visiting' (r, c - 1)
                         (rightCount, cache'') = simulate bounds splitters cache' visiting' (r, c + 1)
                         total = leftCount + rightCount
                         cache''' = Map.insert pos total cache''
                     in (total, cache''')
                else let next = (r + 1, c)
                         (downCount, cache') = simulate bounds splitters cache visiting' next
                         cache'' = Map.insert pos downCount cache'
                     in (downCount, cache'')

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
