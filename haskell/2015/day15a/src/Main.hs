module Main (main) where

import Data.Int (Int32)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Parsec hiding (State)
import Text.Parsec.String
import Control.DeepSeq
import System.Clock

data Ingredient = Ingredient
  { capacity   :: Int
  , durability :: Int
  , flavor     :: Int
  , texture    :: Int
  , calories   :: Int
  }
  deriving (Eq, Show)

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

file :: Parser [Ingredient]
file = line `endBy` newline

number :: Parser Int
number = do
         sign <- option "" (string "-")
         digits <- many1 digit
         return (read (sign ++ digits))

line :: Parser Ingredient
line = do
       _ <- many1 letter >> string ": capacity "
       cap <- number
       _ <- string ", durability "
       dur <- number
       _ <- string ", flavor "
       fla <- number
       _ <- string ", texture "
       tex <- number
       _ <- string ", calories "
       cal <- number
       return Ingredient { capacity = cap
                         , durability = dur
                         , flavor = fla
                         , texture = tex
                         , calories = cal
                         }

distribute :: Int -> Int -> [[Int]]
distribute 0 n = [replicate n 0]
distribute m 1 = [[m]]
distribute m n = [ x:rest | x <- [0..m], rest <- distribute (m - x) (n - 1) ]

score :: [Ingredient] -> [Int] -> Int
score ingredients amounts =
    let totals = foldl1 (zipWith (+))
                     [ [ capacity ing * amt
                       , durability ing * amt
                       , flavor ing * amt
                       , texture ing * amt
                       ]
                     | (ing, amt) <- zip ingredients amounts
                     ]
        [c, d, f, t] = map (max 0) totals
    in c * d * f * t

maxScore :: [Ingredient] -> Int
maxScore ingredients = maximum [ score ingredients alloc | alloc <- distribute 100 (length ingredients) ]

process :: String -> Int32
process content =
    case parse file "" content of
        Left err -> error (show err)
        Right ingredients -> fromIntegral $ maxScore ingredients


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
