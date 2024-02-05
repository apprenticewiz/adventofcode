module Main ( main ) where

import Data.Char
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe
import System.Environment
import System.Exit
import System.IO

splitWhen     :: (Char -> Bool) -> String -> [String]
splitWhen pred s =  case dropWhile pred s of
                      "" -> []
                      s' -> w : splitWhen pred s''
                            where (w, s'') = break pred s'

process :: String -> Int
process contents =
  let instances =
        foldl
          (\currentInstances line ->
            let cardStr = head $ splitWhen (== ':') line
                cardNumber = read (head $ drop 1 $ words cardStr) :: Int
                rest = head $ drop 1 $ splitWhen (== ':') line
                winningStr = head $ splitWhen (== '|') rest
                winningSet = Set.fromList $ (map read (words winningStr) :: [Int])
                handStr = head $ drop 1 $ splitWhen (== '|') rest
                handSet = Set.fromList $ (map read (words handStr) :: [Int])
                commonCount = length (Set.intersection winningSet handSet)
            in
              foldl
                (\prevInstances i ->
                  let copies = (Map.findWithDefault 0 i prevInstances) + 1 + (Map.findWithDefault 0 cardNumber prevInstances)
                  in (Map.insert i copies prevInstances)
                )
                currentInstances
                [(cardNumber + 1)..(cardNumber + commonCount)]
          )
          Map.empty
          (lines contents)
  in (sum $ Map.elems instances) + (length $ lines contents)

usage :: IO a
usage = do
  progname <- getProgName
  putStrLn $ "usage: " ++ progname ++ " <file>"
  exitWith (ExitFailure 1)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> usage
    [filename] -> do
      contents <- readFile filename
      let result = process contents
      putStrLn $ "result = " ++ show result
    _ -> usage
