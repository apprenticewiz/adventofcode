module Main ( main ) where

import Data.IntMap ( IntMap )
import qualified Data.IntMap as IntMap
import Data.List ( sortBy )
import Data.Text ( pack, splitOn, unpack )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure )

usage :: IO ()
usage = do
  progname <- getProgName
  putStrLn $ "usage: " ++ progname ++ " <file>"
  exitFailure

makeRulesTable :: [String] -> IntMap [Int]
makeRulesTable =
    foldl
        (\currentMap ruleTxt ->
            let (first, second) = (takeWhile (/= '|') ruleTxt, tail (dropWhile (/= '|') ruleTxt))
                firstNum = read first
                secondNum = read second
            in IntMap.insertWith (++) firstNum [secondNum] currentMap
        )
        IntMap.empty

processUpdates :: IntMap [Int] -> [[Int]] -> Int
processUpdates rulesTable updates =
    let isValid [x] = True
        isValid (x:xs) = all (\y -> IntMap.member x rulesTable && (y `elem` (rulesTable IntMap.! x))) xs && isValid xs
        comparePages x y
          | x == y = EQ
          | IntMap.member x rulesTable && (y `elem` (rulesTable IntMap.! x)) = LT
          | otherwise = GT
        fixup = sortBy comparePages
    in
        foldl
            (\acc update ->
                if isValid update
                    then acc
                    else
                        let fixedUpdate = fixup update
                            middleIndex = length fixedUpdate `div` 2
                            middleElem = fixedUpdate !! middleIndex
                        in acc + middleElem
            )
            0
            updates

process :: String -> Int
process contents =
    let splitInput inputLines = (takeWhile (/= "") inputLines, tail (dropWhile (/= "") inputLines))
        (rules, updates) = splitInput (lines contents)
        rulesTable = makeRulesTable rules
        updatesList = map (map (read . unpack) . splitOn (pack ",") . pack) updates
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
