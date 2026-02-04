module Main ( main ) where

import Control.Monad
import Control.Monad.State
import Data.Bits
import System.Environment
import System.Exit
import System.IO
import Text.Parsec hiding ( State )
import Text.Parsec.String
import Control.DeepSeq
import System.Clock

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

file :: Parser (Int, Int)
file = do
    a <- generator "A"
    b <- generator "B"
    return (a, b)

generator :: String -> Parser Int
generator x = do
    _ <- string ("Generator " ++ x ++ " starts with ")
    n <- read <$> many1 digit
    _ <- newline
    return n

countMatches :: State (Int, Int, Int) Int
countMatches = do
    replicateM_ 5000000 $ modify (\(a, b, n) ->
            let initA = (a * 16807) `mod` 2147483647
                initB = (b * 48271) `mod` 2147483647
                a' = go initA 4 16807
                b' = go initB 8 48271
                n' = if (a' .&. 0xffff) == (b' .&. 0xffff) then n + 1 else n
            in (a', b', n')
        )
    (_, _, n) <- get
    return n
    where
        go :: Int -> Int -> Int -> Int
        go x multiple factor
            | x `mod` multiple == 0 = x
            | otherwise             = go ((x * factor) `mod` 2147483647) multiple factor

process :: String -> Int
process content =
    case parse file "" content of
        Left err -> error (show err)
        Right (a, b) -> evalState countMatches (a, b, 0)


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
