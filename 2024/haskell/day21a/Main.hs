module Main ( main ) where

import Data.Char ( isDigit )
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
        r1Sequences = map (`findSequences` numericPad) codes
        r2Sequences = map ((`findSequences` dirPad) . concat) r1Sequences
        r3Sequences = map ((`findSequences` dirPad) . concat) r2Sequences
        numbers = map (read . filter isDigit) codes
    in sum $ zipWith (*) (map (length . concat) r3Sequences) numbers

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      contents <- readFile filename
      let result = process contents
      putStrLn $ "result = " ++ show result
    _ -> usage
