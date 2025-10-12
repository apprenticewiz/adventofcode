module Main ( main ) where

import qualified Data.Set as Set
import System.Environment
import System.Exit
import System.IO
import Text.Parsec
import Text.Parsec.String

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

stepPoints :: (Int, Int) -> Direction -> Int -> [(Int, Int)]
stepPoints (x, y) dir n = case dir of
    North -> [(x, y + i) | i <- [1..n]]
    South -> [(x, y - i) | i <- [1..n]]
    East  -> [(x + i, y) | i <- [1..n]]
    West  -> [(x - i, y) | i <- [1..n]]

findFirstRepeat :: [Move] -> (Int, Int)
findFirstRepeat moves = loop (0, 0) North Set.empty moves
  where
    loop pos _ _ [] = pos
    loop pos dir visited (currMove:rest) =
        let (dir', steps) = case currMove of
                MoveLeft n  -> (turnLeft dir, n)
                MoveRight n -> (turnRight dir, n)
            path = stepPoints pos dir' steps
            (visited', repeatPos, finalPos) = foldl step (visited, Nothing, pos) path
        in case repeatPos of
            Just p  -> p
            Nothing -> loop finalPos dir' visited' rest

    step (seen, Just dup, _) _ = (seen, Just dup, undefined)
    step (seen, Nothing, _) p =
        if p `Set.member` seen
            then (seen, Just p, p)
            else (Set.insert p seen, Nothing, p)

process :: String -> Int
process content =
    case parse file "" content of
        Left err -> error (show err)
        Right moves ->
            let firstRepeat = findFirstRepeat moves
            in manhattanDist firstRepeat

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
