module Main ( main ) where

import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure )
import Text.Regex.TDFA ( (=~), AllTextMatches(getAllTextMatches) )

usage :: IO ()
usage = do
  progname <- getProgName
  putStrLn $ "usage: " ++ progname ++ " <file>"
  exitFailure

process :: String -> Int
process contents =
    let extractInsns str = getAllTextMatches (str =~ "mul\\([0-9]+,[0-9]+\\)|don't\\(\\)|do\\(\\)") :: [String]
        extractNumPair mulExpr = getAllTextMatches (mulExpr =~ "[0-9]+") :: [String]
        convertPair numPair = map read numPair :: [Int]
    in fst $ foldl
                (\(acc, enabled) insn ->
                  case insn of
                    "do()" -> (acc, True)
                    "don't()" -> (acc, False)
                    _ -> if enabled
                            then (acc + product (convertPair $ extractNumPair insn), enabled)
                            else (acc, enabled)
                )
                (0, True)
                (extractInsns contents)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      contents <- readFile filename
      let result = process contents
      putStrLn $ "result = " ++ show result
    _ -> usage
