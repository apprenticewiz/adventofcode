module Main ( main ) where

import System.Environment
import System.Exit
import System.IO
import Text.Parsec
import Text.Parsec.String
import Control.DeepSeq
import System.Clock

data Direction = North | East | South | West
                 deriving(Eq, Show)

data Move = MoveLeft Int | MoveRight Int
            deriving (Eq, Show)

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

turnLeft :: Direction -> Direction
turnLeft North = West
turnLeft East  = North
turnLeft South = East
turnLeft West  = South

turnRight :: Direction -> Direction
turnRight North = East
turnRight East  = South
turnRight South = West
turnRight West  = North

file :: Parser [Move]
file = (move `sepBy` (string ", ")) <* (optional newline) <* eof

move :: Parser Move
move = do
       dir <- char 'L' <|> char 'R'
       steps <- many1 digit
       case dir of
           'L' -> return (MoveLeft (read steps))
           'R' -> return (MoveRight (read steps))
           _   -> undefined

manhattanDist :: (Int, Int) -> Int
manhattanDist (x, y) = abs x + abs y

process :: String -> Int
process content =
    case parse file "" content of
        Left err -> error (show err)
        Right moves ->
            let ((x, y), _) = foldl'
                             (\((i, j), d) m ->
                                 let (d', steps) = case m of
                                                       MoveLeft n  -> (turnLeft d, n)
                                                       MoveRight n -> (turnRight d, n)
                                     (i', j') = case d' of
                                                    North -> (i, j + steps)
                                                    East  -> (i + steps, j)
                                                    South -> (i, j - steps)
                                                    West  -> (i - steps, j)
                                 in ((i', j'), d')
                             )
                             ((0, 0), North)
                             moves
             in manhattanDist (x, y)


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
