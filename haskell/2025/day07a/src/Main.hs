module Main ( main ) where

import Control.DeepSeq
import Data.Ix
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
        currentBeams = Set.singleton start
    in Set.size (simulate bounds splitters currentBeams Set.empty)
  where
    simulate :: ((Int, Int), (Int, Int)) -> Set Position -> Set Position -> Set Position -> Set Position
    simulate bounds splitters currentBeams splitLocs
      | Set.null currentBeams = splitLocs
      | otherwise =
            let (nextBeams, nextSplitLocs) =
                    Set.foldr (\beam@(br, bc) (bs, sls) ->
                                   if beam `Set.member` splitters
                                       then let beamLeft = (br, bc - 1)
                                                beamRight = (br, bc + 1)
                                                bs' = Set.union bs (Set.fromList [beamLeft, beamRight])
                                                sls' = Set.union sls (Set.singleton beam)
                                            in (bs', sls')
                                       else if inRange bounds (br + 1, bc)
                                                then (Set.union bs (Set.singleton (br + 1, bc)), sls)
                                                else (bs, sls))
                              (Set.empty, splitLocs)
                              currentBeams
            in simulate bounds splitters nextBeams nextSplitLocs

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
