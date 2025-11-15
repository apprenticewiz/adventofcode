module Main ( main ) where

import Data.List
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import System.Environment
import System.Exit
import System.IO

type Position = (Int, Int)

data Direction = U | D | L | R
                 deriving (Eq, Show)

data Turn = TurnLeft | TurnRight | Straight
            deriving (Eq, Show)

type Track = Map Position Char

data Cart = Cart
  { pos :: Position
  , dir :: Direction
  , nextTurn :: Turn
  }
  deriving (Show)

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

parseInput :: String -> (Track, [Cart])
parseInput content =
    let ls = lines content
        coords = [ ((x, y), c) | (y, row) <- zip [0..] ls
                               , (x, c) <- zip [0..] row ]
    in foldl parseCell (Map.empty, []) coords

parseCell :: (Track, [Cart]) -> (Position, Char) -> (Track, [Cart])
parseCell (track, carts) (p, ch) =
    case ch of
        '^' -> (Map.insert p '|' track, Cart p U TurnLeft : carts)
        'v' -> (Map.insert p '|' track, Cart p D TurnLeft : carts)
        '<' -> (Map.insert p '-' track, Cart p L TurnLeft : carts)
        '>' -> (Map.insert p '-' track, Cart p R TurnLeft : carts)
        ' ' -> (track, carts)
        _   -> (Map.insert p ch track, carts)

move :: Position -> Direction -> Position
move (x, y) U = (x, y - 1)
move (x, y) D = (x, y + 1)
move (x, y) L = (x - 1, y)
move (x, y) R = (x + 1, y)

applyCurve :: Direction -> Char -> Direction
applyCurve d '/' =
    case d of
        U -> R
        D -> L
        L -> D
        R -> U
applyCurve d '\\' =
    case d of
        U -> L
        D -> R
        L -> U
        R -> D
applyCurve _ _ = error "unexpected char seen by applyCurve"

turn :: Direction -> Turn -> Direction
turn d TurnLeft =
    case d of
        U -> L
        L -> D
        D -> R
        R -> U
turn d TurnRight =
    case d of
        U -> R
        L -> U
        D -> L
        R -> D
turn d Straight = d

nextIntersectionTurn :: Turn -> Turn
nextIntersectionTurn TurnLeft = Straight
nextIntersectionTurn Straight = TurnRight
nextIntersectionTurn TurnRight = TurnLeft

stepCart :: Track -> Cart -> Cart
stepCart track (Cart p d t) =
    let p' = move p d
        ch = track Map.! p'
    in case ch of
        '+' -> Cart p' (turn d t) (nextIntersectionTurn t)
        '/' -> Cart p' (applyCurve d '/') t
        '\\' -> Cart p' (applyCurve d '\\') t
        _ -> Cart p' d t

tick :: Track -> [Cart] -> Either Position [Cart]
tick track carts = go [] (sortBy readingOrder carts)
  where
    readingOrder (Cart (x1, y1) _ _) (Cart (x2, y2) _ _) =
        compare (y1, x1) (y2, x2)

    go moved [] = Right (sortBy readingOrder moved)
    go moved (cart:rest) =
        let cart' = stepCart track cart
            allPositions = map pos moved ++ map pos rest
        in if pos cart' `elem` allPositions
            then Left (pos cart')
            else go (cart':moved) rest

findCrash :: Track -> [Cart] -> Position
findCrash track carts =
    case tick track carts of
        Left p -> p
        Right carts' -> findCrash track carts'

process :: String -> (Int, Int)
process content =
    let (track, carts) = parseInput content
    in findCrash track carts

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
