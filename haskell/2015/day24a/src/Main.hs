module Main ( main ) where

import Control.DeepSeq
import Data.List
import Data.Ord
import System.Clock
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, readFile, stderr)

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations k xs
  | k > length xs = []
  | otherwise =
      case xs of
          [] -> []
          (x:rest) ->
              map (x:) (combinations (k-1) rest) ++ combinations k rest

validCombinations :: [Int] -> Int -> Int -> [[Int]]
validCombinations packageWeights numberOfGroups targetWeightPerGroup =
    let packageCount = length packageWeights
        maxPackages = packageCount `div` numberOfGroups
        allValid = concatMap (\n -> filter ((== targetWeightPerGroup) . sum) (combinations n packageWeights))
                             [1..maxPackages]
    in allValid

distributePackages :: [Int] -> Int -> [Int]
distributePackages packages numGroups =
    let totalWeight = sum packages
        targetWeightPerGroup = totalWeight `div` numGroups
        validCombos = validCombinations packages numGroups targetWeightPerGroup
    in case validCombos of
         [] -> error "No valid combination found"
         _  -> minimumBy (comparing product) validCombos

process :: String -> Integer
process content =
    let packages = map read (lines content)
        minGroup = distributePackages packages 3
    in product (map toInteger minGroup)

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
        [filename] -> do
            start <- getTime Monotonic
            content <- readFile filename
            let result = process content
            result `deepseq` return ()
            end <- getTime Monotonic
            let elapsed = diffTimeSpec start end
            putStrLn $ "result = " ++ show result
            putStrLn $ "elapsed time: " ++ showTime elapsed
        _ -> usage progname
