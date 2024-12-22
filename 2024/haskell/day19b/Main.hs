module Main ( main ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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

updateMemo :: Map Text Int -> Text -> Int -> Map Text Int
updateMemo memo design arrangements = Map.insertWith (+) design arrangements memo

possibleArrangements :: [Text] -> Text -> Int -> Map Text Int -> (Int, Map Text Int)
possibleArrangements towels design maxTowelLength memo
    | Map.member design memo = (Map.findWithDefault 0 design memo, memo)
    | Text.null design = (0, memo)
    | otherwise = process (1, memo)
                where
                    process :: (Int, Map Text Int) -> (Int, Map Text Int)
                    process (i, currentMemo)
                        | i > min (Text.length design) maxTowelLength =
                            if not (Map.member design currentMemo)
                                then let currentMemo' = Map.insert design 0 currentMemo
                                     in (currentMemo' Map.! design, currentMemo')
                                else (currentMemo Map.! design, currentMemo)
                        | otherwise =
                            let current = Text.take i design
                            in if current `elem` towels
                                then if Text.length design == i
                                    then process (i + 1, updateMemo currentMemo design 1)
                                    else let (rhsArrangements, currentMemo') = possibleArrangements towels (Text.drop i design) maxTowelLength currentMemo
                                         in process (i + 1, updateMemo currentMemo' design rhsArrangements)
                                else process (i + 1, currentMemo)

countPossible :: [Text] -> [Text] -> Int
countPossible towels designs = 
    let maxTowelLength = maximum (map Text.length towels)  -- Find the maximum towel length
        memo = Map.empty
        totalArrangements = sum $ map (\design -> fst $ possibleArrangements towels design maxTowelLength memo) designs
    in totalArrangements

process :: String -> Int
process contents =
    let (towels, patterns) = parseInput contents
    in countPossible towels patterns

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            contents <- readFile filename
            let result = process contents
            putStrLn $ "result = " ++ show result
        _ -> usage
