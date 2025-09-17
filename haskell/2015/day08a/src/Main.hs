module Main ( main ) where

import Data.Int (Int32)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Parsec
import Text.Parsec.String (Parser)

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

escaped :: Parser Char
escaped = do
    _ <- char '\\'
    choice
        [ char '\\' >> return '\\'
        , char '"' >> return '"'
        , char 'x' >> count 2 hexDigit >> return '?'
        ]

normalChar :: Parser Char
normalChar = noneOf "\\\""

charP :: Parser Char
charP = escaped <|> normalChar

stringLiteralP :: Parser String
stringLiteralP = between (char '"') (char '"') (many charP)

parseLine :: String -> String
parseLine line =
    case parse stringLiteralP "" line of
        Left err -> error (show err)
        Right s -> s

process :: String -> Int32
process content =
    let contentLines = lines content
        codeLens = map length contentLines
        memLens = map (length . parseLine) contentLines
        diffs = zipWith (-) codeLens memLens
    in fromIntegral $ sum diffs

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
