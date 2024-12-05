module Main ( main ) where

import Data.IntMap ( IntMap )
import qualified Data.IntMap as IntMap
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure )

usage :: IO ()
usage = do
  progname <- getProgName
  putStrLn $ "usage: " ++ progname ++ " <file>"
  exitFailure

splitOn :: Char -> String -> [String]
splitOn ch s =
    case dropWhile (== ch) s of
        "" -> []
        s' -> w : splitOn ch s''
            where (w, s'') = break (== ch) s'

makeRulesTable :: [String] -> IntMap [Int]
makeRulesTable =
    foldl
        (\currentMap ruleTxt ->
            let (first, second) = (takeWhile (/= '|') ruleTxt, tail (dropWhile (/= '|') ruleTxt))
                firstNum = read first
                secondNum = read second
            in if IntMap.notMember firstNum currentMap
                then IntMap.insert firstNum [ secondNum ] currentMap
                else
                    let items = currentMap IntMap.! firstNum
                    in IntMap.insert firstNum (items ++ [secondNum]) currentMap
        )
        IntMap.empty

processUpdates :: IntMap [Int] -> [[Int]] -> Int
processUpdates rulesTable updates =
    let isValid [x] = True
        isValid (x:xs) = all (\y -> IntMap.member x rulesTable && (y `elem` (rulesTable IntMap.! x))) xs && isValid xs
    in
        foldl
            (\acc update ->
                if isValid update
                    then
                        let middleIndex = length update `div` 2
                            middleElem = update !! middleIndex
                        in acc + middleElem
                    else acc
            )
            0
            updates

process :: String -> Int
process contents =
    let splitInput inputLines = (takeWhile (/= "") inputLines, tail (dropWhile (/= "") inputLines))
        (rules, updates) = splitInput (lines contents)
        rulesTable = makeRulesTable rules
        updatesList = map (map read . splitOn ',') updates
    in processUpdates rulesTable updatesList

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      contents <- readFile filename
      let result = process contents
      putStrLn $ "result = " ++ show result
    _ -> usage
