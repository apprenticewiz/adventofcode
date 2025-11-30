module Main ( main ) where

import Data.List
import System.Environment
import System.Exit
import System.IO
import Text.Parsec
import Text.Parsec.String

type Position = (Int, Int, Int, Int)

file :: Parser [Position]
file = position `sepEndBy1` newline <* eof

position :: Parser Position
position = do
    x <- integer
    _ <- string ","
    y <- integer
    _ <- string ","
    z <- integer
    _ <- string ","
    w <- integer
    return (x, y, z, w)

integer :: Parser Int
integer = read <$> ((++) <$> option "" (string "-") <*> many1 digit)

manhattan :: Position -> Position -> Int
manhattan (x1, y1, z1, w1) (x2, y2, z2, w2) = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2) + abs (w1 - w2)

growConstellation :: [Position] -> Position -> ([Position], [Position])
growConstellation points seed =
    go [seed] [] points
  where
    go [] cluster rest = (cluster, rest)
    go (p:ps) cluster rest =
        let (near, far) = partition (\q -> manhattan p q <= 3) rest
        in go (ps ++ near) (p:cluster) far

countConstellations :: [Position] -> Int
countConstellations [] = 0
countConstellations (p:ps) =
    let (_, remaining) = growConstellation ps p
    in 1 + countConstellations remaining

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

process :: String -> Int
process content =
    case parse file "" content of
        Left err -> error (show err)
        Right positions -> countConstellations positions

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
