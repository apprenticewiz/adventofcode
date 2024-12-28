module Main ( main ) where

import Data.Char ( isDigit )
import Data.List ( concat )
import Data.Map ( Map )
import qualified Data.Map as Map
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure )

type Position = (Int, Int)

data Keypad = Keypad { coords :: Map Char Position
                     , gap :: Position
                     }

usage :: IO ()
usage = do
  progname <- getProgName
  putStrLn $ "usage: " ++ progname ++ " <file>"
  exitFailure

numericPad :: Keypad
numericPad = Keypad { coords = Map.fromList [ ('7', (0, 0)), ('8', (0, 1)), ('9', (0, 2))
                                            , ('4', (1, 0)), ('5', (1, 1)), ('6', (1, 2))
                                            , ('1', (2, 0)), ('2', (2, 1)), ('3', (2, 2))
                                            ,                ('0', (3, 1)), ('A', (3, 2))
                                            ]
                    , gap = (3, 0)
                    }

dirPad :: Keypad
dirPad = Keypad { coords = Map.fromList [                ('^', (0, 1)), ('A', (0, 2))
                                        , ('<', (1, 0)), ('v', (1, 1)), ('>', (1, 2))
                                        ]
                , gap = (0, 0)
                }

shortestPath :: Char -> Char -> Keypad -> String
shortestPath from to keypad =
    let (r1, c1) = coords keypad Map.! from
        (r2, c2) = coords keypad Map.! to
        ud = if r2 > r1 then replicate (r2 - r1) 'v' else replicate (r1 - r2) '^'
        lr = if c2 > c1 then replicate (c2 - c1) '>' else replicate (c1 - c2) '<'
    in if c2 > c1 && (r2, c1) /= gap keypad
        then ud ++ lr ++ "A"
        else if (r1, c2) /= gap keypad
            then lr ++ ud ++ "A"
            else ud ++ lr ++ "A"

findSequences :: String -> Keypad -> [String]
findSequences keyPresses keypad =
    fst $ foldl
            (\(sequences, prevKey) keyPress ->
                (sequences ++ [shortestPath prevKey keyPress keypad], keyPress)
            )
            ([], 'A')
            keyPresses

process :: String -> Int
process contents =
    let codes = lines contents
        seqCounts sq = foldl (\subMap s -> Map.insertWith (+) s 1 subMap) Map.empty (findSequences sq dirPad)
        initFreqTables = map (\code -> Map.singleton (concat $ findSequences code numericPad) 1) codes
        expandTables = map expandTable
            where
                expandTable = Map.foldrWithKey
                        (\sq freq subFreqTable ->
                            Map.foldrWithKey
                                (\subSeq subFreq subFreqTable' ->
                                    let count = Map.findWithDefault 0 subSeq subFreqTable' + (subFreq * freq)
                                    in Map.insert subSeq count subFreqTable'
                                )
                                subFreqTable
                                (seqCounts sq)
                        )
                        Map.empty
        freqTables = iterate expandTables initFreqTables !! 25
        numbers = map (read . filter isDigit) codes
        complexity = Map.foldrWithKey (\seq freq n -> n + (length seq * freq)) 0
    in sum $ zipWith (*) (map complexity freqTables) numbers

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      contents <- readFile filename
      let result = process contents
      putStrLn $ "result = " ++ show result
    _ -> usage
