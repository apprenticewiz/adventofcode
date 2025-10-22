module Main ( main ) where

import System.Environment
import System.Exit
import System.IO
import Text.Parsec hiding (State)
import Text.Parsec.String

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

file :: Parser [(Int, Int)]
file = line `sepEndBy1` newline <* eof

line :: Parser (Int, Int)
line = do
       _ <- string "Disc #" >> many1 digit >> string " has "
       positions <- read <$> many1 digit
       _ <- string " positions; at time=0, it is at position "
       start <- read <$> many1 digit
       _ <- string "."
       return (positions, start)

process :: String -> Int
process content =
    case parse file "" content of
        Left err -> error (show err)
        Right discs ->
            fst $ foldl'
                (\(t, s) (i, (positions, start)) ->
                    let newT = head [x | x <- [t, t + s ..], (start + x + i) `mod` positions == 0]
                    in (newT, s * positions)
                ) (0, 1) (zip [1..] discs)

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
