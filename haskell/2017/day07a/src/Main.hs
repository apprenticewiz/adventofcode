module Main ( main ) where

import Data.Functor ( (<&>) )
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure )
import System.IO ( hPutStrLn, stderr )
import Text.Parsec ( try, char, digit, letter, newline, spaces, string, between, many1, option, sepBy1, sepEndBy1, parse )
import Text.Parsec.String ( Parser )

data Program = Program
    { weight :: Int
    , name :: String
    , children :: [String]
    } deriving (Show)

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

file :: Parser (Map String Program)
file = (line `sepEndBy1` newline) <&> Map.fromList
  where
    line = do
        n <- many1 letter
        spaces
        w <- between (char '(') (char ')') (many1 digit)
        cs <- option [] (try (spaces >> string "->" >> spaces >> sepBy1 (many1 letter) (string ", ")))
        return (n, Program { weight = read w, name = n, children = cs })

process :: String -> String
process content =
    case parse file "" content of
        Left err -> error (show err)
        Right programs ->
            case [ n | n <- Map.keys programs
                     , all (\p -> n `notElem` children p) (Map.elems programs) ] of
                [root] -> root
                _      -> error "No unique root found"

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
