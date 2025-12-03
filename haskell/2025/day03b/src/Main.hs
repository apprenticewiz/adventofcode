module Main ( main ) where

import Data.List
import Data.Maybe
import System.Environment
import System.Exit
import System.IO

process :: String -> Integer
process content =
    let banks = lines content
    in sum $ map findMaxJolts banks

  where
    findMaxJolts :: String -> Integer
    findMaxJolts = go 12 []

      where
        go :: Int -> String -> String -> Integer
        go numDigits acc s
          | null s || numDigits == 0 = read $ reverse acc
          | otherwise =
            let next = maximum $ take (length s - (numDigits - 1)) s
                nextPos = fromJust $ next `elemIndex` s
            in go (numDigits - 1) (next : acc) (drop (nextPos + 1) s)

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

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
