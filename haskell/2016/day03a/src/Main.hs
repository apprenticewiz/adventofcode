module Main ( main ) where

import Data.List
import System.Environment
import System.Exit
import System.IO

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

process :: String -> Int
process content =
    foldl (\acc line ->
               let nums = (sort $ map read $ words line) :: [Int]
               in if ((nums !! 0) + (nums !! 1)) > (nums !! 2)
                      then acc + 1
                      else acc
          )
          0
          (lines content)

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
