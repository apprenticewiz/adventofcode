module Main ( main ) where

import Data.List ( nub )
import Data.Maybe ( mapMaybe )
import Data.Text (Text)
import qualified Data.Text as Text
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure )

usage :: IO ()
usage = do
    progname <- getProgName
    putStrLn $ "usage: " ++ progname ++ " <file>"
    exitFailure

parseInput :: String -> ([Text], [Text])
parseInput contents =
    let contentLines = lines contents
        towels = Text.splitOn (Text.pack ", ") $ Text.pack $ head contentLines
        patterns = map Text.pack $ drop 2 contentLines
    in (towels, patterns)

reduceTowels :: [Text] -> [Text]
reduceTowels towels =
    let comboTowels s towels = any (\(x, y) -> x <> y == s) pairs
            where
                pairs = [(x, y) | x <- towels, y <- towels, x /= y]
        removeComboTowels towels = filter (`notElem` toRemove) towels
            where
                toRemove = [x | x <- towels, comboTowels x towels]
    in removeComboTowels towels

isValid :: [Text] -> Text -> Bool
isValid towels pattern = checkPattern [(Text.empty, pattern)]
    where
        checkPattern :: [(Text, Text)] -> Bool
        checkPattern !q
            | null q = False
            | otherwise =
                let (matched, toScan) = head q
                    rest = tail q

                    matches = filter (`Text.isPrefixOf` toScan) towels
                    newEntries = map (\match -> (matched <> match, Text.drop (Text.length match) toScan)) matches
                in (any (Text.null . snd) newEntries || checkPattern (nub (rest ++ newEntries)))

countValid :: [Text] -> [Text] -> Int
countValid towels = foldl (\count pattern -> if isValid towels pattern then count + 1 else count) 0

process :: String -> Int
process contents =
    let (towels, patterns) = parseInput contents
    in countValid (reduceTowels towels) patterns

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            contents <- readFile filename
            let result = process contents
            putStrLn $ "result = " ++ show result
        _ -> usage
