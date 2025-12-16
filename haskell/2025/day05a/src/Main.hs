module Main ( main ) where

import Control.DeepSeq
import Data.Range
import System.Clock
import System.Environment
import System.Exit
import System.IO
import Text.Parsec
import Text.Parsec.String

file :: Parser ([Range Int], [Int])
file = do
    freshRanges <- range `sepEndBy1` newline
    _ <- newline
    availableIds <- integer `sepEndBy` newline
    _ <- optional newline >> eof
    return (freshRanges, availableIds)

range :: Parser (Range Int)
range = do
    lo <- integer
    _ <- char '-'
    hi <- integer
    return (lo +=+ hi)

integer :: Parser Int
integer = read <$> many1 digit

process :: String -> Int
process content =
    case parse file "" content of
        Left err -> error (show err)
        Right (freshRanges, availableIds) -> length $ filter (inRanges freshRanges) availableIds

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

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
