module Main ( main ) where

import qualified Data.Text as Text
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure )

usage :: IO ()
usage = do
  progname <- getProgName
  putStrLn $ "usage: " ++ progname ++ " <file>"
  exitFailure

process :: String -> Int
process contents =
    let splitLine line = ((read $ takeWhile (/= ':') line) :: Int, (words . tail) $ dropWhile (/= ':') line)
        generateEquations [x] = [show x]
        generateEquations (x:xs) = [show x ++ " " ++ op ++ " " ++ eqns | op <- ["+", "*"], eqns <- generateEquations xs]
        solve [] = []
        solve [x] = x
        solve (x:op:y:zs) =
            case op of
                "+" -> solve (show (read x + read y):zs)
                "*" -> solve (show (read x * read y):zs)
                _ -> error "unexpected op"
        validSolutions =
            foldl
                (\valid line ->
                    let (lhs, rhs) = splitLine line
                        numberList = map read rhs :: [Int]
                        solutions = map (read . solve . words) (generateEquations numberList)
                    in if lhs `elem` solutions
                        then valid ++ [lhs]
                        else valid
                )
                []
                (lines contents)
    in sum validSolutions

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      contents <- readFile filename
      let result = process contents
      putStrLn $ "result = " ++ show result
    _ -> usage
