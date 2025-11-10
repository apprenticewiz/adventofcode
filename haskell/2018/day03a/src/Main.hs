module Main ( main ) where

import qualified Data.Map.Strict as Map
import System.Environment
import System.Exit
import System.IO
import Text.Parsec
import Text.Parsec.String

type Position = (Int, Int)

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

file :: Parser [(Position, Position)]
file = line `sepEndBy1` newline <* eof

line :: Parser (Position, Position)
line = do
   _ <- char '#' >> many1 digit >> string " @ "
   x <- read <$> many1 digit
   _ <- char ','
   y <- read <$> many1 digit
   _ <- string ": "
   w <- read <$> many1 digit
   _ <- char 'x'
   h <- read <$> many1 digit
   return ((x, y), (x + w - 1, y + h - 1))

process :: String -> Int
process content =
    case parse file "" content of
        Left err -> error (show err)
        Right claims ->
             let counts = foldr (\((ulx, uly), (lrx, lry)) acc -> 
                                     let points = [ (x, y) | x <- [ulx..lrx], y <- [uly..lry] ]
                                     in foldr (\p acc' -> Map.insertWith (+) p 1 acc') acc points
                                ) Map.empty claims
             in Map.size $ Map.filter (> 1) counts

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
