module Main (main) where

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

element :: Parser String
element = do
          first <- upper
          rest  <- many lower
          return (first : rest)

molecule :: Parser [String]
molecule = many1 element <* eof

process :: String -> Int32
process content =
    let moleculeLine = last (lines content)
    in case parse molecule "" moleculeLine of
           Left err -> error (show err)
           Right elts -> fromIntegral $ countSteps elts
  where
    countSteps :: [String] -> Int
    countSteps elts =
        let n    = length elts
            rnAr = length [t | t <- elts, t == "Rn" || t == "Ar"]
            ys   = length [t | t <- elts, t == "Y"]
        in n - rnAr - 2 * ys - 1

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
