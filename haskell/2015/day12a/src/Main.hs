module Main ( main ) where

import Data.Int (Int32)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Parsec
import Text.Parsec.String

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

number :: Parser Int
number = do
         sign <- option "" (string "-")
         digits <- many1 digit
         return (read (sign ++ digits))

numbers :: Parser [Int]
numbers =
    skipMany (noneOf "-0123456789") *>
    many (number <* skipMany (noneOf "-0123456789"))
    <* eof

process :: String -> Int32
process content =
    case parse numbers "" content of
        Left err -> error (show err)
        Right ns -> fromIntegral $ sum ns

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
