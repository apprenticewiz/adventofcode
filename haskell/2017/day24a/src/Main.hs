module Main ( main ) where

import Data.List.Split
import System.Environment
import System.Exit
import System.IO

type Component = (Int, Int)

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

scanComponents :: String -> [Component]
scanComponents content = map scanComponent (lines content)
  where
    scanComponent line =
        let [a, b] = map read $ splitOn "/" line
        in (a, b)

buildBridges :: [(Int, Int)] -> Int -> [[(Int, Int)]]
buildBridges components port =
    [ comp : rest | comp@(a, b) <- components
                  , port == a || port == b
                  , let nextPort = if port == a then b else a
                  , rest <- buildBridges (remove comp components) nextPort
    ] ++ [[]]
  where
    remove x = filter (/= x)

strength :: [(Int, Int)] -> Int
strength = sum . map (uncurry (+))

process :: String -> Int
process content =
    let components = scanComponents content
    in maximum [ strength bridge | bridge <- buildBridges components 0 ]

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
