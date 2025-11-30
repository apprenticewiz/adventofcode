module Main ( main ) where

import Data.List
import Data.Ord
import System.Environment
import System.Exit
import System.IO
import Text.Parsec
import Text.Parsec.String

data Bot = Bot
  { bx :: Int
  , by :: Int
  , bz :: Int
  , br :: Int
  } deriving (Show)

file :: Parser [Bot]
file = botParser `sepEndBy` newline <* eof

botParser :: Parser Bot
botParser = do
    _ <- string "pos=<"
    xVal <- integer
    _ <- string ","
    yVal <- integer
    _ <- string ","
    zVal <- integer
    _ <- string ">, r="
    rVal <- integer
    return (Bot xVal yVal zVal rVal)

integer :: Parser Int
integer = read <$> ((++) <$> option "" (string "-") <*> many1 digit)

manhattan :: Bot -> Bot -> Int
manhattan a b = abs (bx a - bx b) + abs (by a - by b) + abs (bz a - bz b)

countInRange :: Bot -> [Bot] -> Int
countInRange strongest bots = length [ b | b <- bots, manhattan strongest b <= br strongest ]

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

process :: String -> Int
process content =
    case parse file "" content of
        Left err -> error (show err)
        Right bots ->
            let strongest = maximumBy (comparing br) bots
            in countInRange strongest bots

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
