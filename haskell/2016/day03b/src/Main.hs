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
    let triples = regroupColumns $ transformInput ([], [], []) $ lines content
    in foldl (\acc triple -> 
                  let sorted = sort triple
                  in if (sorted !! 0) + (sorted !! 1) > (sorted !! 2) then acc + 1 else acc
             )
             0
             triples
  where
    transformInput :: ([Int], [Int], [Int]) -> [String] -> ([Int], [Int], [Int])
    transformInput xs [] = xs 
    transformInput (as, bs, cs) (line:rest) =
        let ns = (map read $ words line) :: [Int]
        in transformInput (as ++ [ns !! 0], bs ++ [ns !! 1], cs ++ [ns !! 2]) rest

    regroupColumns :: ([Int], [Int], [Int]) -> [[Int]]
    regroupColumns (as, bs, cs) =
        let ns = as ++ bs ++ cs
            genTriples [] xs = xs
            genTriples [_] xs = xs
            genTriples [_, _] xs = xs
            genTriples (a:b:c:rest) xs = genTriples rest ([a, b, c]:xs)
        in reverse $ genTriples ns []

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
