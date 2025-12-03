module Main ( main ) where

import System.Environment
import System.Exit
import System.IO

process :: String -> Int
process content =
    let banks = lines content
    in sum $ map findMaxJolts banks

  where
    findMaxJolts :: String -> Int
    findMaxJolts s =
        let first = maximum $ take (length s - 1) s
            s' = drop 1 $ dropWhile (/= first) s
            second = maximum s'
        in read [first, second]

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
