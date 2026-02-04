module Main ( main ) where

import Data.Char
import Data.List
import System.Environment
import System.Exit
import System.IO
import Control.DeepSeq
import System.Clock

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

process :: String -> Int
process contents =
    let steps = map parseStep (lines contents)
        q = (nub . sort) [ a | (a, _) <- steps, not (any (\(_, b) -> a == b) steps) ]
    in simulate steps [] q [] 0
  where
    parseStep :: String -> (Char, Char)
    parseStep line =
        let ws = words line
        in (head (ws !! 1), head (ws !! 7))

    duration :: Char -> Int
    duration c = 60 + (ord c - ord 'A' + 1)

    simulate :: [(Char, Char)] -> String -> [Char] -> [(Char, Int)] -> Int -> Int
    simulate steps done q workers time
        | length done == length allSteps = time - 1
        | otherwise =
            let
                workers' = [ (c, t - 1) | (c, t) <- workers ]
                newlyDone = [ c | (c, t) <- workers', t == 0 ]
                done' = done ++ newlyDone
                stillWorking = [ (c, t) | (c, t) <- workers', t > 0 ]
                ready = [ b | (a, b) <- steps
                            , a `elem` done'
                            , all (`elem` done') [ x | (x, y) <- steps, y == b ]
                            , b `notElem` done'
                            , b `notElem` map fst stillWorking
                        ]
                q' = (nub . sort) ((q ++ ready) \\ newlyDone)
                numFree = 5 - length stillWorking
                toAssign = take numFree q'
                newWork = [ (c, duration c) | c <- toAssign ]
                q'' = q' \\ toAssign
            in simulate steps done' q'' (stillWorking ++ newWork) (time + 1)
      where
        allSteps = (nub . sort) ([ a | (a, _) <- steps ] ++ [ b | (_, b) <- steps ])


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
