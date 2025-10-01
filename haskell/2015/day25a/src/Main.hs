module Main ( main ) where

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, readFile, stderr)
import Text.Parsec
import Text.Parsec.String

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

file :: Parser (Int, Int)
file = do
       _ <- string "To continue, please consult the code grid in the manual.  Enter the code at row "
       row <- many1 digit
       _ <- string ", column "
       col <- many1 digit
       _ <- string "."
       optional newline >> eof
       return (read row, read col)

generate :: (Int, Int) -> Integer
generate (r, c) 
  | r == 1 && c == 1 = 20151125 :: Integer
  | r > 1 && c == 1  = step (generate (1, r - 1))
  | otherwise        = step (generate (r + 1, c - 1))
  where
    step n = (n * 252533) `mod` 33554393

process :: String -> Integer
process content =
    case parse file "" content of
        Left err -> error (show err)
        Right (row, col) -> generate (row, col)

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
