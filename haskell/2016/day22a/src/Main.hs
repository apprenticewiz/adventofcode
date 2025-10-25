module Main ( main ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict ( Map )
import qualified Data.Set as Set
import System.Environment
import System.Exit
import System.IO
import Text.Parsec
import Text.Parsec.String

type Coordinate = (Int, Int)

data Node = Node { size :: Int, used :: Int } deriving (Show)

type Grid = Map Coordinate Node

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <filename> <input string>"
    exitFailure

file :: Parser Grid
file = do
    _ <- manyTill anyChar newline >> manyTill anyChar newline
    rows <- many line <* eof
    return $ Map.fromList rows

line :: Parser (Coordinate, Node)
line = do
    _ <- string "/dev/grid/node-"
    _ <- char 'x'
    x <- read <$> many1 digit
    _ <- char '-'
    _ <- char 'y'
    y <- read <$> many1 digit
    _ <- spaces
    sizeVal <- read <$> many1 digit
    _ <- char 'T' >> spaces
    usedVal <- read <$> many1 digit
    _ <- char 'T' >> manyTill anyChar newline
    return ((x, y), Node sizeVal usedVal)

process :: String -> Int
process content =
    case parse file "" content of
        Left err -> error (show err)
        Right grid -> countViablePairs grid
    where
        countViablePairs :: Grid -> Int
        countViablePairs g = (Set.size . Set.fromList)
            [ (coordA, coordB)
            | (coordA, nodeA) <- Map.toList g,
              (coordB, nodeB) <- Map.toList g,
              coordA /= coordB,
              used nodeA > 0,
              used nodeA <= (size nodeB - used nodeB)
            ]

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
