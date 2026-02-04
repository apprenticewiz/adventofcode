module Main ( main ) where

import Control.DeepSeq
import Data.List
import System.Clock
import System.Environment
import System.Exit
import System.IO
import Text.Parsec
import Text.Parsec.String

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

file :: Parser [([String], [String])]
file = (line `sepEndBy` newline) <* eof

line :: Parser ([String], [String])
line = do
       ts <- many1 (insideBrackets <|> outsideBrackets)
       let (outside, inside) =
               foldr (\t (o, i) ->
                          case t of
                              '[':_ -> (o, (init (tail t)):i)
                              _ -> (t:o, i)
                     )
                     ([], [])
                     ts
       return (outside, inside)

insideBrackets :: Parser String
insideBrackets = do
                 l <- string "["
                 s <- many1 letter
                 r <- string "]"
                 return (l ++ s ++ r)

outsideBrackets :: Parser String
outsideBrackets = many1 letter

process :: String -> Int
process content =
    case parse file "" content of
        Left err -> error (show err)
        Right addrs -> foldr (\(o, i) acc -> if isSsl (o, i) then acc + 1 else acc) 0 addrs
  where
    isSsl :: ([String], [String]) -> Bool
    isSsl (os, is) = any (\o -> hasAbaAndBab o is) os

    hasAbaAndBab o is
        | length o < 3 = False
        | otherwise = case o of
                          a:b:c:_ | a == c && a /= b && any (\i -> [b, a, b] `isInfixOf` i) is -> True
                          _ -> hasAbaAndBab (tail o) is

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
