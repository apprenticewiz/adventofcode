module Main ( main ) where

import System.Environment
import System.Exit
import System.IO
import Control.DeepSeq
import System.Clock

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

computeValue :: Node -> Int
computeValue (Node kids md) =
    case kids of
        [] -> sum md
        _ ->
            let selectedKids = [ kids !! (n - 1) | n <- md, n <= length kids ]
            in sum $ map computeValue selectedKids

process :: String -> Int
process content =
    let input = map read (words content) :: [Int]
        (root, _) = parseNode input
    in computeValue root


showTime :: TimeSpec -> String
showTime elapsed =
    let ns = fromIntegral (toNanoSecs elapsed) :: Double
    in if ns < 1000
       then show ns ++ " ns"
       else if ns < 1000000
       then show (ns / 1000.0) ++ " μs"
       else if ns < 1000000000
            then show (ns / 1000000.0) ++ " ms"
            else show (ns / 1000000000.0) ++ " s"
main :: IO ()
main = do
    args <- getArgs
    progname <- getProgName
    case args of
        [filename] -> do
            start <- getTime Monotonic
            content <- readFile filename
            let result = process content
            result `deepseq` return ()
            end <- getTime Monotonic
            let elapsed = diffTimeSpec start end
            putStrLn $ "result = " ++ show result
            putStrLn $ "elapsed time: " ++ showTime elapsed
        _ -> usage progname
