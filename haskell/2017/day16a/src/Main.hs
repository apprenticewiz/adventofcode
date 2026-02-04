module Main ( main ) where

import Control.DeepSeq
import Control.Monad
import Control.Monad.State
import Data.Array
import System.Clock
import System.Environment
import System.Exit
import System.IO
import Text.Parsec hiding ( State )
import Text.Parsec.String

data Move = Spin Int
          | Exchange Int Int
          | Partner Char Char
          deriving (Eq, Show)

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

file :: Parser [Move]
file = moveParser `sepBy` char ','

moveParser :: Parser Move
moveParser = spin <|> exchange <|> partner

spin :: Parser Move
spin = do
    _ <- char 's'
    x <- read <$> many1 digit
    return (Spin x)

exchange :: Parser Move
exchange = do
    _ <- char 'x'
    a <- read <$> many1 digit
    _ <- char '/'
    b <- read <$> many1 digit
    return (Exchange a b)

partner :: Parser Move
partner = do
    _ <- char 'p'
    a <- letter
    _ <- char '/'
    b <- letter
    return (Partner a b)

dance :: [Move] -> State (Array Int Char) String
dance moves = do
    forM_ moves $ \move ->
        modify (\progArray -> case move of
                                Spin n -> handleSpin n progArray
                                Exchange a b -> handleExchange a b progArray
                                Partner a b -> handlePartner a b progArray)
    gets elems
    where
        handleSpin n arr =
            let toMove = 16 - n
                xs = elems arr
            in listArray (0, 15) (drop toMove xs ++ take toMove xs)

        handleExchange a b arr =
            let la = arr ! a
                lb = arr ! b
            in arr // [(a, lb), (b, la)]

        handlePartner a b arr =
            let [na] = [ n | (n, x) <- assocs arr, x == a ]
                [nb] = [ n | (n, x) <- assocs arr, x == b ]
            in arr // [(na, b), (nb, a)]

process :: String -> String
process content =
    case parse file "" content of
        Left err -> error (show err)
        Right moves -> evalState (dance moves) (listArray (0, 15) ['a'..'p'])

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
