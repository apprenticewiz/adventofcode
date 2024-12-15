module Main ( main ) where

import Data.Ratio ( (%), denominator, numerator )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure )
import Text.Regex.TDFA ( (=~), AllTextMatches(getAllTextMatches) )

type Position = (Int, Int)

type Velocity = (Int, Int)

data Robot = Robot { position :: Position
                   , velocity :: Velocity
                   } deriving (Show)

usage :: IO ()
usage = do
    progname <- getProgName
    putStrLn $ "usage: " ++ progname ++ " <file> <width> <height>"
    exitFailure

parseInput :: [String] -> [Robot]
parseInput = map parseLine
    where parseLine line =
            let re = "p=([0-9]+),([0-9]+) v=([-]?[0-9]+),([-]?[0-9]+)"
                (_, _, _, vals) = line =~ re  :: (String, String, String, [String])
            in Robot { position = (read (head vals), read (vals !! 1))
                     , velocity = (read (vals !! 2), read (vals !! 3))}

step :: (Int, Int) -> [Robot] -> [Robot]
step (width, height) = map moveRobot
    where moveRobot robot =
            let (x, y) = position robot
                (dx, dy) = velocity robot
                wrap (x, y) = (wrapX, wrapY)
                    where wrapX
                            | x < 0 = x + width
                            | x > width - 1 = x - width
                            | otherwise = x
                          wrapY
                            | y < 0 = y + height
                            | y > height - 1 = y - height
                            | otherwise = y
            in Robot { position = wrap (x + dx, y + dy)
                     , velocity = (dx, dy)
                     }

calcFactor :: (Int, Int) -> [Robot] -> Int
calcFactor (width, height) robots =
    let midX = width `div` 2
        midY = height `div` 2
        nw = [ r | r <- robots, fst (position r) < midX, snd (position r) < midY ]
        ne = [ r | r <- robots, fst (position r) > midX, snd (position r) < midY ]
        se = [ r | r <- robots, fst (position r) > midX, snd (position r) > midY ]
        sw = [ r | r <- robots, fst (position r) < midX, snd (position r) > midY ]
    in length nw * length ne * length se * length sw

process :: String -> Int -> Int -> Int
process contents width height =
    let robots = parseInput (lines contents)
        doStep = step (width, height)
        runSteps n = iterate doStep robots !! n
    in calcFactor (width, height) (runSteps 100)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename, width, height] -> do
            contents <- readFile filename
            let result = process contents (read width) (read height)
            putStrLn $ "result = " ++ show result
        _ -> usage
