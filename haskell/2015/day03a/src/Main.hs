module Main ( main ) where

import Data.Int (Int32)
import Data.Set as Set
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, readFile, stderr)

import AOC_Utils.Geometry.Position2D (Position2D(..))

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
