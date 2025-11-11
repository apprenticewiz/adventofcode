module Main ( main ) where

import Data.Char
import System.Environment
import System.Exit
import System.IO

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure        

reducePolymer :: String -> String
reducePolymer = foldl react []
  where
    react (x:xs) y
      | x /= y && toLower x == toLower y = xs
    react xs y = y:xs

process :: String -> Int
process content =
    let polymer = if last content == '\n' then init content else content
    in length $ reducePolymer polymer

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
