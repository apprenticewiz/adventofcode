module Main (main) where

import Data.Int (Int32)
import Data.List
import qualified Data.Map.Strict as Map
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Parsec
import Text.Parsec.String

type HappinessTable = Map.Map (String, String) Int

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

file :: Parser [(String, String, Int)]
file = line `endBy` newline

line :: Parser (String, String, Int)
line = do
       person1 <- many1 letter
       _ <- string " would "
       action <- string "gain" <|> string "lose"
       spaces
       amountTxt <- many1 digit
       _ <- string " happiness units by sitting next to "
       person2 <- many1 letter
       _ <- string "."
       let amount = case action of
                        "gain" -> read amountTxt
                        "lose" -> -(read amountTxt)
                        _ -> 0
       return (person1, person2, amount)

buildHapTable :: [(String, String, Int)] -> HappinessTable
buildHapTable = foldl (\acc (p1, p2, h) -> Map.insert (p1, p2) h acc) Map.empty

computeScore :: HappinessTable -> [String] -> Int
computeScore hapTable people = computeSeats 0 0
  where
    computeSeats n score
      | n == length people = score
      | otherwise          = 
            let person = people !! n
                left = if n == 0
                           then people !! (length people - 1)
                           else people !! (n - 1)
                right = if n == (length people - 1)
                           then people !! 0
                           else people !! (n + 1)
                leftScore = hapTable Map.! (person, left)
                rightScore = hapTable Map.! (person, right)
            in computeSeats (n + 1) (score + leftScore + rightScore)

process :: String -> Int32
process content =
    case parse file "" content of
        Left err -> error (show err)
        Right arrangements ->
            let hapTable = buildHapTable arrangements
                people = nub $ map (\(x, _, _) -> x) arrangements
            in fromIntegral $ maximum $ map (computeScore hapTable) $ permutations people

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
