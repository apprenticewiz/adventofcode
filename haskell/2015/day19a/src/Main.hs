module Main (main) where

import Data.Int (Int32)
import Data.List
import qualified Data.Set as Set
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Parsec
import Text.Parsec.String

type Rule = (String, String)

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

file :: Parser ([Rule], String)
file = do
       rules <- rule `endBy` newline
       _ <- newline
       molecule <- many1 letter <* newline <* eof
       return (rules, molecule)

rule :: Parser Rule
rule = do
       lhs <- many1 letter
       _ <- string " => "
       rhs <- many1 letter
       return (lhs, rhs)

process :: String -> Int32
process content =
    case parse file "" content of
        Left err -> error (show err)
        Right (rules, molecule) -> fromIntegral $ Set.size $ distinctMolecules rules molecule
  where
    distinctMolecules :: [Rule] -> String -> Set.Set String
    distinctMolecules rules molecule = Set.fromList $ concatMap (`applyRule` molecule) rules

    applyRule :: Rule -> String -> [String]
    applyRule (lhs, rhs) molecule = [ pre ++ rhs ++ suf | (pre, rest) <- zip (inits molecule) (tails molecule),
                                                          lhs `isPrefixOf` rest,
                                                          let suf = drop (length lhs) rest ]

main :: IO ()
main = do
    args <- getArgs
    progname <- getProgName
    case args of
        [filename] -> do
            content <- readFile filename
            let result = process content
            putStrLn $ "result = " ++ show result
        _ -> usage progname
