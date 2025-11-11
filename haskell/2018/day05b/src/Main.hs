module Main ( main ) where

import Data.Char
import Data.List
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
        components = (nub . sort . map toLower) polymer
    in minimum $ map (\x -> length $ reducePolymer (removeComponent x polymer)) components
  where
    removeComponent ch = filter ((/= ch) . toLower)

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
