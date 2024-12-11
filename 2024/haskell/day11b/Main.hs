module Main ( main ) where

import Data.IntMap ( IntMap )
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe ( isJust, fromJust )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure )

usage :: IO ()
usage = do
  progname <- getProgName
  putStrLn $ "usage: " ++ progname ++ " <file>"
  exitFailure

initialize :: String -> IntMap Int
initialize contents =
    foldl
        (\stoneMap stone ->
            IntMap.insert stone 1 stoneMap
        )
        IntMap.empty
        (map read (words contents))

blink :: IntMap Int -> IntMap Int
blink = IntMap.foldrWithKey
            (\stone count newStones ->
                let len = length (show stone)
                    addOrInsertDefault key cnt intMap =
                        let currentValue = IntMap.findWithDefault 0 key intMap
                        in IntMap.insert key (currentValue + cnt) intMap
                in if stone == 0
                    then addOrInsertDefault 1 count newStones
                    else if even len
                        then let firstHalf = stone `div` (10 ^ (len `div` 2))
                                 secondHalf = stone `mod` (10 ^ (len `div` 2))
                             in addOrInsertDefault firstHalf count
                                  (addOrInsertDefault secondHalf count newStones)
                        else addOrInsertDefault (stone * 2024) count newStones
        )
        IntMap.empty

process :: String -> Int
process contents =
    let stones = initialize contents
    in sum $ IntMap.elems (iterate blink stones !! 75)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      contents <- readFile filename
      let result = process contents
      putStrLn $ "result = " ++ show result
    _ -> usage
