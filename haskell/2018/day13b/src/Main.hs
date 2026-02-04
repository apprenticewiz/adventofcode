module Main ( main ) where

import Data.List
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import System.Environment
import System.Exit
import System.IO
import Control.DeepSeq
import System.Clock

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
    in foldl' parseCell (Map.empty, []) coords

parseCell :: (Track, [Cart]) -> (Position, Char) -> (Track, [Cart])
parseCell (!track, !carts) (p, ch) =
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
    let !p' = move p d
        !ch = track Map.! p'
    in case ch of
        '+' -> let !d' = turn d t
                   !t' = nextIntersectionTurn t
               in Cart p' d' t'
        '/' -> let !d' = applyCurve d '/'
               in Cart p' d' t
        '\\' -> let !d' = applyCurve d '\\'
                in Cart p' d' t
        _ -> Cart p' d t

tick :: Track -> [Cart] -> [Cart]
tick track carts = 
    let !cartMap = Map.fromList [(pos c, c) | c <- carts]
        !sortedPositions = sortBy (\(x1, y1) (x2, y2) -> compare (y1, x1) (y2, x2)) (Map.keys cartMap)
        !result = go Map.empty cartMap sortedPositions
    in Map.elems result
  where
    go !moved _ [] = moved
    go !moved !remaining (p:rest) =
        case Map.lookup p remaining of
            Nothing -> go moved remaining rest
            Just cart ->
                let !cart' = stepCart track cart
                    !p' = pos cart'
                    !remaining' = Map.delete p remaining
                in if Map.member p' moved
                    then let !moved' = Map.delete p' moved
                         in go moved' remaining' rest
                    else if Map.member p' remaining'
                        then let !remaining'' = Map.delete p' remaining'
                             in go moved remaining'' rest
                        else let !moved' = Map.insert p' cart' moved
                             in go moved' remaining' rest

findLastCart :: Track -> [Cart] -> Position
findLastCart track carts =
    case carts of
        [c] -> pos c
        _ -> let !carts' = tick track carts
             in findLastCart track carts'

process :: String -> (Int, Int)
process content =
    let (track, carts) = parseInput content
    in findLastCart track carts


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
