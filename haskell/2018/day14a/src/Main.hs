module Main ( main ) where

import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import System.Environment
import System.Exit
import System.IO

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input>"
    exitFailure

type State = (Seq Int, Int, Int)

generateRecipes :: Int -> State -> State
generateRecipes needed state@(recipes, pos1, pos2)
    | Seq.length recipes >= needed = state
    | otherwise =
        let !val1 = Seq.index recipes pos1
            !val2 = Seq.index recipes pos2
            !sum' = val1 + val2
            !recipes' = if sum' >= 10
                       then recipes |> 1 |> (sum' `mod` 10)
                       else recipes |> sum'
            !len = Seq.length recipes'
            !pos1' = (pos1 + val1 + 1) `mod` len
            !pos2' = (pos2 + val2 + 1) `mod` len
        in generateRecipes needed (recipes', pos1', pos2')

process :: String -> String
process input =
    let n = read input :: Int
        initialRecipes = Seq.fromList [3, 7]
        (finalRecipes, _, _) = generateRecipes (n + 10) (initialRecipes, 0, 1)
        result = take 10 $ drop n $ foldr (:) [] finalRecipes
    in concatMap show result

main :: IO ()
main = do
    args <- getArgs
    progname <- getProgName
    case args of
        [input] -> do
            let result = process input
            putStrLn $ "result = " ++ result
        _ -> usage progname
