module Main ( main ) where

import Data.Char ( isDigit )
import Data.List ( isSuffixOf )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure )

usage :: IO ()
usage = do
    progname <- getProgName
    putStrLn $ "usage: " ++ progname ++ " <file>"
    exitFailure

parseInput :: String -> [(Int, [Int])]
parseInput contents =
    foldl
        (\acc line ->
            let nums = words line
                result = read $ filter isDigit (head nums)
                operands = reverse $ map read $ drop 1 nums
            in acc ++ [(result, operands)]
        )
        []
        (lines contents)

validExpr :: (Int, [Int]) -> Bool
validExpr (total, operands)
    | length operands == 1 = head operands == total
    | otherwise =
        let totalStr = show total
            headOpStr = show (head operands)
            canDiv = total `mod` head operands == 0
            canSub = total >= head operands
            canCat = headOpStr `isSuffixOf` totalStr
        in (not (not canDiv && not canSub && not canCat) &&
            ((canDiv && validExpr (total `div` head operands, tail operands)) ||
             (canSub && validExpr (total - head operands, tail operands)) ||
             (canCat && validExpr (read $ "0" ++ take (length totalStr - length headOpStr) totalStr, tail operands))))

process :: String -> Int
process contents =
    let input = parseInput contents
    in sum $ map fst $ filter validExpr input

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            contents <- readFile filename
            let result = process contents
            putStrLn $ "result = " ++ show result
        _ -> usage
