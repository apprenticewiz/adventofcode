module Main ( main ) where

import Control.DeepSeq
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import System.Clock
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

showTime :: TimeSpec -> String
showTime elapsed =
    let ns = fromIntegral (toNanoSecs elapsed) :: Double
    in if ns < 1000
       then show ns ++ " ns"
       else if ns < 1000000
       then show (ns / 1000.0) ++ " μs"
       else if ns < 1000000000
            then show (ns / 1000000.0) ++ " ms"
            else show (ns / 1000000000.0) ++ " s"

main :: IO ()
main = do
    args <- getArgs
    progname <- getProgName
    case args of
        [input] -> do
            start <- getTime Monotonic
            let result = process input
            result `deepseq` return ()
            end <- getTime Monotonic
            let elapsed = diffTimeSpec start end
            putStrLn $ "result = " ++ result
            putStrLn $ "elapsed time: " ++ showTime elapsed
        _ -> usage progname
