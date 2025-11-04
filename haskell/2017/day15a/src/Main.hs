module Main ( main ) where

import Control.Monad
import Control.Monad.State
import Data.Bits
import System.Environment
import System.Exit
import System.IO
import Text.Parsec hiding ( State )
import Text.Parsec.String

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

file :: Parser (Int, Int)
file = do
    a <- generator "A"
    b <- generator "B"
    return (a, b)

generator :: String -> Parser Int
generator x = do
    _ <- string ("Generator " ++ x ++ " starts with ")
    n <- read <$> many1 digit
    _ <- newline
    return n

countMatches :: State (Int, Int, Int) Int
countMatches = do
    replicateM_ 40000000 $ modify (\(a, b, n) ->
            let a' = (a * 16807) `mod` 2147483647
                b' = (b * 48271) `mod` 2147483647
                n' = if (a' .&. 0xffff) == (b' .&. 0xffff) then n + 1 else n
            in (a', b', n')
        )
    (_, _, n) <- get
    return n

process :: String -> Int
process content =
    case parse file "" content of
        Left err -> error (show err)
        Right (a, b) -> evalState countMatches (a, b, 0)

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
