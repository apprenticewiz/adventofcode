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
    let extractMulExprs str = getAllTextMatches (str =~ "mul\\([0-9]+,[0-9]+\\)") :: [String]
        extractNumPair mulExpr = getAllTextMatches (mulExpr =~ "[0-9]+") :: [String]
        convertPair numPair = map read numPair :: [Int]
    in sum $ map ((product . convertPair) . extractNumPair) (extractMulExprs contents)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      contents <- readFile filename
      let result = process contents
      putStrLn $ "result = " ++ show result
    _ -> usage
