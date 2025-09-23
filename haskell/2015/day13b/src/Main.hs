module Main (main) where

import Data.Int (Int32)
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
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
buildHapTable = foldl' (\acc (p1, p2, h) -> Map.insert (p1, p2) h acc) Map.empty

injectSelf :: HappinessTable -> [String] -> HappinessTable
injectSelf hapTable people =
    foldl' (\acc person -> Map.insert ("Self", person) 0 (Map.insert (person, "Self") 0 acc)) hapTable people

computeScore :: HappinessTable -> [String] -> Int
computeScore hapTable people =
    sum [ hapTable Map.! (p, l) + hapTable Map.! (p, r) | (p, l, r) <- triples people ]
  where
    triples xs = zip3 xs (last xs : init xs) (tail xs ++ [head xs])

process :: String -> Int32
process content =
    case parse file "" content of
        Left err -> error (show err)
        Right arrangements ->
            let people = Set.toList $ Set.fromList [ p1 | (p1, _, _) <- arrangements ]
                hapTable = injectSelf (buildHapTable arrangements) people
                (fixed:rest) = people ++ ["Self"]
            in fromIntegral $ maximum $ map (computeScore hapTable . (fixed:)) (permutations rest)

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
