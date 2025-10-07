module Main (main) where

import Data.Int (Int32)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Parsec
import Text.Parsec.String

type Sue = Map.Map String Int

type PossibleSet = Set.Set Int

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

file :: Parser [(Int, Sue)]
file = line `endBy` newline

trait :: Parser (String, Int)
trait = do
        thing <- choice [ try (string "children")
                        , try (string "cats")
                        , string "samoyeds"
                        , try (string "pomeranians")
                        , string "akitas"
                        , string "vizslas"
                        , string "goldfish"
                        , string "trees"
                        , string "cars"
                        , string "perfumes"
                        ]
        amount <- read <$> (string ": " *> many1 digit)
        return (thing, amount)

line :: Parser (Int, Sue)
line = do
       n <- read <$> (string "Sue " *> many1 digit <* string ": ")
       (thing1, amount1) <- trait <* string ", "
       (thing2, amount2) <- trait <* string ", "
       (thing3, amount3) <- trait
       return (n, Map.fromList [(thing1, amount1), (thing2, amount2), (thing3, amount3)])

possibleSet :: [(Int, Sue)] -> String -> Int -> PossibleSet
possibleSet sues thing amount = Set.fromList [ n | (n, sue) <- sues, isPossible sue thing amount ]
  where
    isPossible s "cats" a = case Map.lookup "cats" s of
                                Nothing -> True
                                Just n  -> (n > a)
    isPossible s "trees" a = case Map.lookup "trees" s of
                                Nothing -> True
                                Just n  -> (n > a)
    isPossible s "pomeranians" a = case Map.lookup "pomeranians" s of
                                       Nothing -> True
                                       Just n  -> (n < a)
    isPossible s "goldfish" a = case Map.lookup "goldfish" s of
                                       Nothing -> True
                                       Just n  -> (n < a)
    isPossible s t a = case Map.lookup t s of
                              Nothing -> True
                              Just n  -> (a == n)

process :: String -> Int32
process content =
    case parse file "" content of
        Left err -> error (show err)
        Right sues -> fromIntegral $ Set.elemAt 0 $
            foldl'
                (\acc (thing, amount) -> acc `Set.intersection` (possibleSet sues thing amount))
                (Set.fromList (map fst sues))
                [("children", 3), ("cats", 7), ("samoyeds", 2), ("pomeranians", 3), ("akitas", 0),
                 ("vizslas", 0), ("goldfish", 5), ("trees", 3), ("cars", 2), ("perfumes", 1)]

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
