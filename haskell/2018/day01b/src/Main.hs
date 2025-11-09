module Main ( main ) where

import qualified Data.Set as Set
import System.Environment
import System.Exit
import System.IO

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

process :: String -> Int
process content = step Set.empty 0 $ cycle $ map (read . filter (/= '+')) $ lines content
  where
    step _ _ [] = error "unreachable"
    step seen freq (n:ns) =
        if Set.member freq seen
            then freq
            else step (Set.insert freq seen) (freq + n) ns

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
