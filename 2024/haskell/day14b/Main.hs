module Main ( main ) where

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
    putStrLn $ "usage: " ++ progname ++ " <file>"
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

displayGrid :: (Int, Int) -> [Robot] -> String
displayGrid (width, height) robots =
    let positions = map position robots
    in unlines [ [if (x, y) `elem` positions then '*' else '.' | x <- [0..(width - 1)]] | y <- [0..(height - 1)]]

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            contents <- readFile filename
            let robots = parseInput (lines contents)
                width = 101
                height = 103
                doStep = step (width, height)
                numSteps = 6243
                runSteps = iterate doStep robots !! numSteps
            putStrLn ("Steps: " ++ show numSteps ++ "\n" ++ displayGrid (width, height) runSteps)
        _ -> usage
