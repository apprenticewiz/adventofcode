module Main ( main ) where

import Data.Int (Int32)
import Data.List as List
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
process contents = fromIntegral $ Set.size finalMoves
    where
        (finalMoves, _, _, _) = state
        state = Prelude.foldl processMove (Set.singleton initPos, initPos, initPos, True) moves
        moves = intercalate "" $ lines contents
        initPos = Position2D { x = 0, y = 0 }
        processMove (positions, santa, roboSanta, santaMove) ch =
            if santaMove then (let newSanta = case ch of
                                                    '^' -> Position2D { x = x santa, y = y santa + 1 }
                                                    'v' -> Position2D { x = x santa, y = y santa - 1 }
                                                    '<' -> Position2D { x = x santa - 1, y = y santa }
                                                    '>' -> Position2D { x = x santa + 1, y = y santa }
                                                    _ -> santa
                               in (Set.insert newSanta positions, newSanta, roboSanta, False))
                         else (let newRoboSanta = case ch of
                                                    '^' -> Position2D { x = x roboSanta, y = y roboSanta + 1 }
                                                    'v' -> Position2D { x = x roboSanta, y = y roboSanta - 1 }
                                                    '<' -> Position2D { x = x roboSanta - 1, y = y roboSanta }
                                                    '>' -> Position2D { x = x roboSanta + 1, y = y roboSanta }
                                                    _ -> roboSanta
                               in (Set.insert newRoboSanta positions, santa, newRoboSanta, True))

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
