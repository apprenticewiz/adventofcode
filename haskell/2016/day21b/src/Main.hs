module Main ( main ) where

import Data.List
import Data.Maybe
import System.Environment
import System.Exit
import System.IO
import Text.Parsec
import Text.Parsec.String
import Control.DeepSeq
import System.Clock

data Operation =
    SwapPositions Int Int
  | SwapLetters Char Char
  | RotateLeft Int
  | RotateRight Int
  | Move Int Int
  | RotatePosition Char
  | Reverse Int Int
  deriving (Eq, Show)

input :: String
input = "fbgdceah"

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <filename> <input string>"
    exitFailure

file :: Parser [Operation]
file = (line `sepEndBy1` newline) <* eof

line :: Parser Operation
line = choice
    [ try swapParser
    , try rotateParser
    , try moveParser
    , reverseParser
    ]

swapParser :: Parser Operation
swapParser = string "swap " *> (byPosition <|> byLetter)
  where
    byPosition = do
        from <- read <$> (string "position " *> many1 digit)
        to <- read <$> (string " with position " *> many1 digit)
        return (SwapPositions from to)
    byLetter = do
        a <- string "letter " *> letter
        b <- string " with letter " *> letter
        return (SwapLetters a b)

rotateParser :: Parser Operation
rotateParser = string "rotate " *> (bySteps <|> byLetter)
  where
    bySteps = do
      dir <- string "left" <|> string "right"
      n <- read <$> (string " " *> many1 digit)
      _ <- string " step" >> optional (char 's')
      return $ if dir == "left" then RotateLeft n else RotateRight n
    byLetter = RotatePosition <$> (string "based on position of letter " *> letter)

moveParser :: Parser Operation
moveParser = do
    from <- read <$> (string "move position " *> many1 digit)
    to <- read <$> (string " to position " *> many1 digit)
    return (Move from to)

reverseParser :: Parser Operation
reverseParser = do
    from <- read <$> (string "reverse positions " *> many1 digit)
    to <- read <$> (string " through " *> many1 digit)
    return (Reverse from to)

applyOp :: String -> Operation -> String
applyOp str op =
    case op of
        SwapPositions from to -> swap from to str
        SwapLetters first second -> swap (pos first) (pos second) str
            where
                pos c = fromJust (elemIndex c str)
        RotateLeft n -> rotateLeft n str
        RotateRight n -> rotateRight n str
        Move from to ->
            let ch = str !! from
                without = take from str ++ drop (from + 1) str
            in take to without ++ [ch] ++ drop to without
        RotatePosition ch ->
            let chPos = fromJust (elemIndex ch str)
                n = (if chPos < 4 then chPos + 1 else chPos + 2) `mod` length str
            in drop (length str - n) str ++ take (length str - n) str
        Reverse from to ->
            let (lo, hi) = if from <= to then (from, to) else (to, from)
                prefix = take lo str
                middle = take (hi - lo + 1) (drop lo str)
                suffix = drop (hi + 1) str
            in prefix ++ reverse middle ++ suffix
    where
        swap :: Int -> Int -> [a] -> [a]
        swap i j xs =
            let a = xs !! i
                b = xs !! j
                replace k c ys = take k ys ++ [c] ++ drop (k + 1) ys
            in replace j a (replace i b xs)

rotateLeft, rotateRight :: Int -> String -> String
rotateLeft n s = let n' = n `mod` length s in drop n' s ++ take n' s
rotateRight n s = rotateLeft (length s - n) s

invertOp :: Operation -> String -> String
invertOp op str =
    case op of
        SwapPositions from to -> applyOp str (SwapPositions from to)
        SwapLetters first second -> applyOp str (SwapLetters first second)
        RotateLeft n -> applyOp str (RotateRight n)
        RotateRight n -> applyOp str (RotateLeft n)
        Move from to -> applyOp str (Move to from)
        RotatePosition ch -> head [ candidate
                                    | i <- [0 .. length str - 1]
                                    , let candidate = rotateLeft i str
                                    , applyOp candidate (RotatePosition ch) == str
                                  ]
        Reverse from to -> applyOp str (Reverse from to)

process :: String -> String
process content =
    either (error . show) (foldr invertOp input) (parse file "" content)


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
            putStrLn $ "result = " ++ result
            putStrLn $ "elapsed time: " ++ showTime elapsed
        _ -> usage progname
