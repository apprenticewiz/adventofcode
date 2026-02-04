module Main ( main ) where

import Data.Int (Int32)
import Data.Set as Set
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, readFile, stderr)

import AOC_Utils.Geometry.Position2D (Position2D(..))
import Control.DeepSeq
import System.Clock

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

process :: String -> Int32
process contents = fromIntegral $ Set.size $ fst $ Prelude.foldl processMove (Set.singleton initSanta, initSanta) contents
    where
        initSanta = Position2D { x = 0, y = 0 }
        processMove (positions, santa) ch =
            let newSanta = case ch of
                            '^' -> Position2D { x = x santa, y = y santa + 1 }
                            'v' -> Position2D { x = x santa, y = y santa - 1 }
                            '<' -> Position2D { x = x santa - 1, y = y santa }
                            '>' -> Position2D { x = x santa + 1, y = y santa }
                            _ -> santa
            in (Set.insert newSanta positions, newSanta)


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
