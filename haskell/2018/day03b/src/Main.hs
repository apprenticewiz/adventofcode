module Main ( main ) where

import qualified Data.Map.Strict as Map
import System.Environment
import System.Exit
import System.IO
import Text.Parsec
import Text.Parsec.String
import Control.DeepSeq
import System.Clock

type Position = (Int, Int)

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

file :: Parser [(Position, Position)]
file = line `sepEndBy1` newline <* eof

line :: Parser (Position, Position)
line = do
   _ <- char '#' >> many1 digit >> string " @ "
   x <- read <$> many1 digit
   _ <- char ','
   y <- read <$> many1 digit
   _ <- string ": "
   w <- read <$> many1 digit
   _ <- char 'x'
   h <- read <$> many1 digit
   return ((x, y), (x + w - 1, y + h - 1))

process :: String -> Int
process content =
    case parse file "" content of
        Left err -> error (show err)
        Right claims ->
             let counts = foldr (\((ulx, uly), (lrx, lry)) acc -> 
                                     let points = [ (x, y) | x <- [ulx..lrx], y <- [uly..lry] ]
                                     in foldr (\p acc' -> Map.insertWith (+) p 1 acc') acc points
                                ) Map.empty claims
             in findIntact counts (zip [1..] claims)
  where
    findIntact cs claims =
        fst $ head $ filter (\(n, ((ulx, uly), (lrx, lry))) ->
                                let points = [ (x, y) | x <- [ulx..lrx], y <- [uly..lry] ]
                                in all (\p -> cs Map.! p == 1) points) claims


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
