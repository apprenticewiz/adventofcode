module Main ( main ) where

import System.Environment
import System.Exit
import System.IO

data Node = Node { children :: [Node], metadata :: [Int] }
            deriving (Show)

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

parseNode :: [Int] -> (Node, [Int])
parseNode (numChildren:numMetadata:rest) =
    let (kids, rest') = parseChildren numChildren rest
        (md, rest'') = splitAt numMetadata rest'
    in (Node kids md, rest'')
parseNode _ = error "expected more data"

parseChildren :: Int -> [Int] -> ([Node], [Int])
parseChildren 0 xs = ([], xs)
parseChildren n xs =
    let (child, rest) = parseNode xs
        (others, rest') = parseChildren (n - 1) rest
    in (child : others, rest')

metadataSum :: Node -> Int
metadataSum (Node kids md) = sum md + sum (map metadataSum kids)

process :: String -> Int
process content =
    let input = map read (words content) :: [Int]
        (root, _) = parseNode input
    in metadataSum root

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
